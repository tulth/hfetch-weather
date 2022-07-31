module Main where

import GHC.IO.Encoding ( utf8, setLocaleEncoding )
import System.IO ( hSetBuffering, stdout, BufferMode( NoBuffering ) )

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import System.Process ( createProcess, proc )
import Control.Concurrent ( threadDelay )
import Data.Functor ( (<&>) )
import Data.Maybe ( listToMaybe )
import Data.List ( intercalate )
import Data.Either.Combinators ( maybeToRight, whenRight, whenLeft, fromRight )
import Data.Time

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad ( MonadPlus(mzero) )
import Control.Monad.Trans.Except ( runExceptT, ExceptT(..))
import Control.Monad.IO.Class ( MonadIO(liftIO) )
-- import Control.Exception

import Network.HTTP.Simple
    ( defaultRequest,
      getResponseBody,
      getResponseStatusCode,
      httpLBS,
      HttpException(..),
      setRequestHeader,
      setRequestHost,
      setRequestMethod,
      setRequestPath,
      setRequestPort,
      setRequestSecure,
      Request,
      Response )
import Data.Aeson
    ( eitherDecode, FromJSON(parseJSON), Value(Object), (.:) )
import qualified Data.Aeson as J
import Data.Aeson.Lens ( key, AsPrimitive(_String), _Double)
import Control.Lens ( (^?) ) -- conflict with a Data.Aeson operator.
import Network.HTTP.Client.Conduit (HttpException(InvalidUrlException))
import Control.Exception ( try, Exception )
import Control.Exception.Base (displayException)
import GHC.IO.Exception (IOException(IOError))

data ConfigRecord = ConfigRecord
  { apiWebHost :: !String
  , apiWebHostPath :: !String
  , apiWebHostKey :: !String
  , iconWebHost :: !String
  , iconWebHostPath :: !String
  , requestSecure :: Bool
  , requestPort :: Int
  , cacheBasePath :: !String
  , apiWebHostLatitude :: Double
  , apiWebHostLongitude :: Double
  , sleepSeconds :: Int
  } deriving (Show)

instance FromJSON ConfigRecord where
  parseJSON (Object v) =
    ConfigRecord <$> v .: "apiWebHost"
                 <*> v .: "apiWebHostPath"
                 <*> v .: "apiWebHostKey"
                 <*> v .: "iconWebHost"
                 <*> v .: "iconWebHostPath"
                 <*> v .: "requestSecure"
                 <*> v .: "requestPort"
                 <*> v .: "cacheBasePath"
                 <*> v .: "apiWebHostLatitude"
                 <*> v .: "apiWebHostLongitude"
                 <*> v .: "sleepSeconds"
    -- A non-Object value is of the wrong type, so use mzero to fail.
  parseJSON _          = mzero

convertExceptionToString :: Exception e => IO (Either e a) -> IO (Either String a)
convertExceptionToString = fmap convertExceptionToStringHelper

convertExceptionToStringHelper :: Exception e => Either e a -> Either String a
convertExceptionToStringHelper = either (Left . convertExceptionToStringHelperHelper) Right

convertExceptionToStringHelperHelper :: Exception e => e -> String
convertExceptionToStringHelperHelper = show :: Exception e => e -> String


buildRequest :: String -> String
             -> String -> Bool -> Int -> Request
buildRequest host method path isSecure port =
  setRequestMethod (BC.pack method)
  $ setRequestHost (BC.pack host)
  $ setRequestPath (BC.pack path)
  $ setRequestSecure isSecure
  $ setRequestPort port
  defaultRequest

jsonGetKey :: T.Text -> Value -> Either String Value
jsonGetKey k json = maybeToRight
  ("json error parsing for key >" ++ show k ++ "< ")
  (json ^? key k)

jsonGetCurrently :: Value -> Either String Value
jsonGetCurrently = jsonGetKey "currently"

jsonGetCurrentlyIcon :: Value -> Either String T.Text
jsonGetCurrentlyIcon json = jsonGetCurrently json
                            >>= jsonGetKey "icon"
                            >>= \v -> maybeToRight
                                       "json error while getting icon as string, failed to cast to string"
                                       ((^? _String) v)

jsonGetCurrentlyDoubleKey :: String -> Value -> Either String Double
jsonGetCurrentlyDoubleKey doubleKey json = jsonGetCurrently json
                                           >>= jsonGetKey (T.pack doubleKey)
                                           >>= \v -> maybeToRight
                                                     ("json error while getting "
                                                      ++ doubleKey
                                                      ++ " as double, failed to cast")
                                                     ((^? _Double) v)

jsonGetCurrentlyTemperature :: Value -> Either String Double
jsonGetCurrentlyTemperature = jsonGetCurrentlyDoubleKey "temperature"

doHttpRequestBody :: Request -> ExceptT String IO LC.ByteString
doHttpRequestBody request = do
  response <- ExceptT . convertExceptionToString . try @HttpException $ httpLBS request
  let status = getResponseStatusCode response
  let result = (
        if status == 200
        then Right $ getResponseBody response :: Either String LC.ByteString
        else Left $ "request failed with bad status "
             ++ show status
             ++ " request: "
             ++ show request
        )
  ExceptT $ return result

fetchIconName :: Request -> ExceptT String IO String
fetchIconName request = do
  responseBody <- doHttpRequestBody request
  ExceptT $ return $ apiResponseBodyToIconName responseBody

safeGetResponseBody :: Response LC.ByteString -> Either String LC.ByteString
safeGetResponseBody response
  | status == 200 =
    Right $ getResponseBody response :: Either String LC.ByteString
  | otherwise = Left $ "request failed with bad status " ++ show status
  where status = getResponseStatusCode response

apiResponseBodyToIconName :: LC.ByteString -> Either String String
apiResponseBodyToIconName responseBody =
  eitherDecode responseBody
  >>= jsonGetCurrentlyIcon
  >>= Right . T.unpack

apiResponseBodyGetDoubleKey :: LC.ByteString -> String -> Either String Double
apiResponseBodyGetDoubleKey responseBody doubleKey =
  eitherDecode responseBody
  >>= jsonGetCurrentlyDoubleKey doubleKey

fetchIconData :: Request -> ExceptT String IO LC.ByteString
fetchIconData = doHttpRequestBody

doDoesFileExist :: String -> ExceptT String IO Bool
doDoesFileExist name = ExceptT . convertExceptionToString . try @IOError $ doesFileExist name

doReadFileLBS :: String -> ExceptT String IO LC.ByteString
doReadFileLBS name = ExceptT . convertExceptionToString . try @IOError $ L.readFile name

doWriteFile :: String -> String -> ExceptT String IO ()
doWriteFile name content = ExceptT . convertExceptionToString . try @IOError $ writeFile name content

doWriteFileLBS :: String -> LC.ByteString -> ExceptT String IO ()
doWriteFileLBS name content = ExceptT . convertExceptionToString . try @IOError $ L.writeFile name content

handleCacheIconFile :: Bool -> Request -> String -> ExceptT String IO String
handleCacheIconFile existP request cacheIconFile =
  if existP
  then return $ "File " ++ cacheIconFile ++ " already exists; skipping write"
  else do
    iconData <- fetchIconData request
    doWriteFileLBS cacheIconFile iconData
    return ("Wrote " ++ show (LC.length iconData) ++ " bytes of iconData to " ++ cacheIconFile)

convertToXpmIconFile :: Bool -> String -> String -> ExceptT String IO String
convertToXpmIconFile existP inName outName =
  if existP
  then return $ "File " ++ outName ++ " already exists; skipping convert"
  else do
    r <- liftIO $ createProcess (proc "convert"
                                  (words "-alpha on -scale 28x28"  ++ [inName] ++ [outName]))
    return ("Converted " ++ inName ++ " to " ++ outName)

createInfoCsv :: String -> Integer -> Integer -> Integer -> Integer -> String
createInfoCsv icon temp hum pressure windSpeed =
  strJoin "," $ icon : [show temp, show hum, show pressure, show windSpeed]

formApiRequestPath :: ConfigRecord -> String
formApiRequestPath cfg =
  strJoin "/" (filter (not . null) [apiWebHostPath cfg, apiWebHostKey cfg, latLong])
  where latLong = strJoin "," [
          show $ apiWebHostLatitude cfg,
          show $ apiWebHostLongitude cfg]

formIconRequestPath :: ConfigRecord -> String -> String
formIconRequestPath cfg iconName  =
  strJoin "/" (filter  (not . null) [iconWebHostPath cfg, iconName])

strJoin :: String -> [String] -> String
strJoin = intercalate

createInfoHtml :: String -> Integer -> Integer -> Integer -> Integer -> String -> String -> String
createInfoHtml icon temp hum pressure windSpeed utcTime zonedTime =
  unlines [ "<!DOCTYPE html>"
          , "<html>"
          , "  <head>"
          , "    <title>Weather Mirror</title>"
          , "    <style>"
          , "      html { color-scheme: light dark; }"
          , "      body { width: 35em; margin: 0 auto;"
          , "             font-family: Tahoma, Verdana, Arial, sans-serif; }"
          , "    </style>"
          , "  </head>"
          , "  <body>"
          , "    <h1>Weather Mirror</h1>"
          , "    <img src=\""++ icon ++ "\" alt=\""++ icon ++ "\">"
          , "    <br/>"
          , "    Temperature: " ++ show temp ++ "<span>&#176;</span>F"
          , "    <br/>"
          , "    Humidity: " ++ show hum ++ "%"
          , "    <br/>"
          , "    Pressure: " ++ show pressure ++ "mb"
          , "    <br/>"
          , "    Wind: " ++ show windSpeed ++ "mph"
          , "    <br/>"
          , "    Fetched on: "
          , "    <br/>"
          , "    " ++ utcTime
          , "    <br/>"
          , "    " ++ zonedTime
          , "    <br/>"
          , "  </body>"
          , "</html>"
          ]



prog :: ConfigRecord -> ExceptT String IO ()
prog cfg = do
  responseBody <- doHttpRequestBody (buildRequest
                                     (apiWebHost cfg)
                                     "GET"
                                     (formApiRequestPath cfg)
                                     (requestSecure cfg)
                                     (requestPort cfg))
  let cacheApiFile = cacheBasePath cfg ++ "cache-api-response.json"
  doWriteFileLBS cacheApiFile responseBody
  liftIO $ putStrLn $ "Wrote cached api json response to " ++ cacheApiFile
  iconName <- ExceptT $ return $ apiResponseBodyToIconName responseBody
  temperature <- round <$> ExceptT (return $ apiResponseBodyGetDoubleKey responseBody "temperature")
  humidity <- round . (* 100) <$> ExceptT (return $ apiResponseBodyGetDoubleKey responseBody "humidity")
  pressure <- round <$> ExceptT (return $ apiResponseBodyGetDoubleKey responseBody "pressure")
  windSpeed <- round <$> ExceptT (return $ apiResponseBodyGetDoubleKey responseBody "windSpeed")
  liftIO $ putStrLn $ "temp: " ++ show temperature
                   ++ " humidity: " ++ show humidity
                   ++ " pressure: " ++ show pressure
                   ++ " windSpeed: " ++ show windSpeed
  let iconFileName = iconName ++ ".png"
  let cacheIconFile = cacheBasePath cfg ++ iconFileName
  cacheIconFileExists <- doDoesFileExist cacheIconFile
  let hostIconPath = iconWebHostPath cfg ++ iconFileName
  handleCacheIconFileResult <- handleCacheIconFile cacheIconFileExists (
    buildRequest (iconWebHost cfg) "GET" (formIconRequestPath cfg iconFileName)
    (requestSecure cfg)
    (requestPort cfg)) cacheIconFile
  liftIO $ putStrLn handleCacheIconFileResult
  --
  let xpmIconFile = iconName ++ ".xpm"
  let xpmIconPath = cacheBasePath cfg ++ xpmIconFile
  let xmobarString = createInfoCsv xpmIconFile temperature humidity pressure windSpeed
  liftIO $ putStrLn xmobarString
  let xmobarFile = cacheBasePath cfg ++ "weather.csv"
  doWriteFile xmobarFile xmobarString
  --
  utcTime <- liftIO getCurrentTime
  zonedTime <- liftIO getZonedTime
  liftIO $ putStrLn $ "utcTime:   " ++ show utcTime
  liftIO $ putStrLn $ "zonedTime: " ++ show zonedTime
  let htmlString = createInfoHtml iconFileName temperature humidity pressure
                   windSpeed (show utcTime) (show zonedTime)
  let htmlFile = cacheBasePath cfg ++ "index.html"
  doWriteFile htmlFile htmlString
  --
  xpmIconFileExists <- doDoesFileExist xpmIconFile
  convertToXpmIconFileResult <- convertToXpmIconFile xpmIconFileExists cacheIconFile xpmIconPath
  liftIO $ putStrLn convertToXpmIconFileResult
  return ()

mainloop :: ConfigRecord -> IO ()
mainloop cfg = do
  result <- runExceptT $ prog cfg
  whenLeft result putStrLn
  let sleepSec = sleepSeconds cfg
  putStrLn $ unwords ["sleeping for:", show sleepSec, "seconds"]
  threadDelay $ sleepSec * 1000000
  mainloop cfg

getConfigFileFromArgs :: [String] -> Either String String
getConfigFileFromArgs args =
  maybeToRight "getConfigFileFromArgs: Unable to parse command line arguments" (listToMaybe args)

getConfigFromBytes :: LC.ByteString -> ExceptT String IO ConfigRecord
getConfigFromBytes content = ExceptT $ pure $ eitherDecode content

getConfig :: ExceptT String IO ConfigRecord
getConfig = do
  configFilePath <- ExceptT $ getArgs <&> getConfigFileFromArgs
  liftIO $ putStrLn $ "Loading configuration from " ++ configFilePath
  configFileContents <- doReadFileLBS configFilePath
  getConfigFromBytes configFileContents

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout NoBuffering
  config <- runExceptT getConfig
  whenLeft config print
  whenRight config mainloop
