module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Concurrent (forkIO, runInUnboundThread)

clientMaxBodySize :: Int
clientMaxBodySize = 8388608 -- 8mb

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 9999 iNADDR_ANY)
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = runInUnboundThread $ do
    conn <- accept sock
    putStr $ "Forking for connection from " ++ (show . snd $ conn) ++ "..."
    forkIO (handleConn conn)
    putStrLn " Done."
    mainLoop sock

handleConn :: (Socket, SockAddr) -> IO ()
handleConn (sock, _) = do
    send sock $ C.pack "what should i echo: "
    input <- liftM C.unpack $ recv sock clientMaxBodySize
    send sock . C.pack $ "okay.\n" ++ input ++ "bye\n"
    close sock

