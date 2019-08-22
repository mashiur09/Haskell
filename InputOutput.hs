
{-# LANGUAGE GADTSyntax #-}

--import Control.Monad   
--import System.Random
--import Data.Char


 main0 :: IO ()
 main0 = do 
     putStrLn "Give first number: "
     number1 <- getLine
     putStrLn "Give second number: "
     number2 <- getLine
     let list = number1 ++ number2
     putStrLn ("Reverse value: " ++ show (reverse list) ++ " Actual value: " ++ show(list) )

   
 main1 :: IO ()
 main1 = do 
     putStrLn "Give first number: "
     number1 <- getLine
     putStrLn "Give second number: "
     number2 <- getLine
     let list = number1 ++ number2
     putStrLn ("Reverse value: " ++ show (reverse list) ++ " Actual value: " ++ show(list) ++ " max value " ++ (show (max number1 number2)))
 


 main2 :: IO ()
 main2 = do 
     putStrLn "Give first number: "
     number1 <- getLine
     let n1 = read (number1) :: Int
     let str = (drawSpecialBox n1)

     putStrLn ( show (str))        


 drawSpecialBox :: Int -> String
 drawSpecialBox 0 = "" 
 drawSpecialBox n = take n (repeat (' ')) ++ "\n"  ++ take n (repeat ('*')) ++ "\n" ++ take n (repeat (' ')) 

 drawSpecialBox1 :: Int -> String
 drawSpecialBox1 0 = "" 
 drawSpecialBox1 n = take n (repeat (' ')) ++ take n (repeat ('*')) ++ take n (repeat (' ')) 



 main3 :: IO ()
 main3 = do
     putStrLn ("1st user already provided a guess word.")
     let guessWord = "Password"
     let str = map (\x -> '*') guessWord
     putStrLn (str)
     letTheGamesBegin guessWord 0

 letTheGamesBegin :: String -> Int -> IO ()
 letTheGamesBegin guessWord tryNo = do
     putStrLn ("Try to guess a letter of the secret word")
     guessLetter <- getChar
     if(guessLetter `elem` guessWord) then 
         do   
         putStrLn ("You have guessed correctly with " ++ show (tryNo +1 ) ++ " guesses")
         return ()
     else
         do
         putStrLn ("please try again")
         letTheGamesBegin guessWord (tryNo+1)    


 mapString :: Char -> String -> String
 mapString _ [] = []
 mapString c (x:xs) | c == x    = x   : mapString c xs 
                     | otherwise = '*' : mapString c xs   

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
 hangman :: String -> IO ()
 hangman secret = do
         putStrLn ("Secret")
         let str = map (\x -> '*') secret
         putStrLn (str)
         letTheHangmanGamesBegin secret [] []  

 letTheHangmanGamesBegin :: String -> [Char] -> [Char] -> IO ()
 letTheHangmanGamesBegin guessWord ch1 ch2 = do             
         putStrLn (" Try to guess a letter of the secret word :: ")
         guessChar <- getChar
         if (elem guessChar ['a'..'z'] || elem guessChar ['A'..'Z']) then
             if elem guessChar guessWord && not (elem guessChar ch2) 
                 then updatePosition guessWord ch1 (guessChar:ch2)
                 else updatePosition guessWord (guessChar:ch1) ch2
         else 
             letTheHangmanGamesBegin guessWord ch1 ch2


 updatePosition :: String -> [Char] -> [Char] -> IO ()
 updatePosition guessWord ch1 ch2 = do 
         putStr "\n"
         putStr "Secret : "
         let newSecret = map (\c ->  if c `elem` ch2 then c else '*') guessWord
         putStrLn(newSecret)
         if elem '*' newSecret
         then if (length ch1) == 6
             then putStrLn("Lost this game.")
             else letTheHangmanGamesBegin guessWord ch1 ch2
         else 
             putStrLn("Solved in " ++ (show(length ch1 + length ch2)) ++ " tries" )
                         

---------------------------------------------------------------------------------------------------------------------------------------------------------------------

 star :: Int -> Int -> String
 star len n  | n == 1    = "*"
             | n == len  = "* " ++ (star len (n-1))
             | otherwise = "  " ++ (star len (n-1))


 generateNewStar :: Int -> [String]
 generateNewStar 0 = [] 
 generateNewStar n = take ((n*n)+n+n+n)  (([(show n)] ++ [" "] ++ (take n (repeat "*")) ++ ["\n"] ) ++ (generateNewStar n) )  




 generateStar :: Int -> [String]
 generateStar 0 = []
 generateStar n =  ([(show n)] ++  (take n (repeat "*")) ++ ["\n"] ) ++ generateStar (n-1)  

 printStar :: IO ()
 printStar = do
     putStrLn "Give a number: "
     starNo <- getLine
     let starString = concat (generateStar (read starNo :: Int))
     putStrLn (starString) 

         
 printStarStraight :: IO ()
 printStarStraight = do
     putStrLn "Give a number: "
     starNo <- getLine
     let starString = concat (generateNewStar (read starNo :: Int))
     putStrLn (starString) 


---------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 printTree :: IO ()
 printTree = do
     putStrLn "Give a number: "
     starNo <- getLine
     if((read starNo :: Int) >= 3) then
         do 
         let starString = concat (setTree (read starNo :: Int))
         putStrLn (starString)
     else 
         do
         putStrLn "please give a number which is greater than 2"
         printTree
 
 setTree :: Int -> [String]
 setTree n = (designTreeTop n (n-1) 1) ++ (designTreeBottom n (n-1) 1)


 designTreeBottom :: Int -> Int -> Int -> [String] 
 designTreeBottom totalLine initialDifference initialStar | totalLine == 3 = (designTree initialDifference initialStar initialDifference)
                                                          | otherwise      = (designTree initialDifference initialStar initialDifference) ++ (designTree initialDifference initialStar initialDifference)


 designTreeTop :: Int -> Int -> Int -> [String]
 designTreeTop    0       initialDifference initialStar = []
 designTreeTop totalLine initialDifference initialStar = designTree initialDifference initialStar initialDifference ++ 
                                                                   designTreeTop (totalLine -1) (initialDifference -1) (initialStar +2)
                                                                                

 
 designTree :: Int -> Int -> Int -> [String]
 designTree left center right | (left /= 0) && (right /=0) = take left (repeat " ") ++ take center (repeat "*") ++ take right (repeat " ") ++ ["\n"]   
                              | otherwise = take center (repeat "*") ++ ["\n"]                               

--------------------------------------------------------------------------------------------------------------------------------------------------------------

 




 setDiamond :: Int -> [String]
 setDiamond n = (designDiamondBottom (n-1) 1  (n + (n-1) -2)) ++ (designDiamondTop n (n-1) 1) 

 designDiamondBottom :: Int -> Int -> Int -> [String] 
 designDiamondBottom    0      initialDifference initialStar = []
 designDiamondBottom totalLine initialDifference initialStar = designDiamond initialDifference (initialStar) initialDifference  
                                                                  ++ designDiamondBottom (totalLine -1) (initialDifference + 1) (initialStar-2)


 designDiamondTop :: Int -> Int -> Int -> [String]
 designDiamondTop    0       initialDifference initialStar = []
 designDiamondTop totalLine  initialDifference initialStar = designDiamond initialDifference initialStar initialDifference ++ 
                                                                 designDiamondTop (totalLine -1) (initialDifference -1) (initialStar +2)
                                                                             


 designDiamond :: Int -> Int -> Int -> [String]
 designDiamond left center right | (left /= 0) && (right /=0) = take left (repeat " ") ++ take center (repeat "*") ++ take right (repeat " ") ++ ["\n"]   
                                 | otherwise = take center (repeat "*") ++ ["\n"]    


 printDiamond :: IO ()
 printDiamond = do
     putStrLn "Give a number: "
     starNo <- getLine
     if((read starNo :: Int) >1 ) then
         do 
         let starString = concat (setDiamond (read starNo :: Int))
         putStrLn (starString)
     else if((read starNo :: Int) == 1 ) then
         do
         putStrLn ("*")        
     else 
         do
         putStrLn "please give a number which is greater than 1"
         printDiamond

---------------------------------------------------------------------------------------------------------------------------------------------------------



 

 printVerticalLine :: IO ()
 printVerticalLine = do
     putStrLn "Give a line length : "
     lineLength <- getLine
     let string = concat (drawVerticalLine (read (lineLength) :: Int))
     putStrLn (string)    


 drawVerticalLine :: Int -> [String]
 drawVerticalLine 0 = []
 drawVerticalLine n = (take 1 (repeat ("*"))) ++ ["\n"] ++ drawVerticalLine (n - 1)  
     

 printHorizontalLine :: IO ()
 printHorizontalLine = do
     putStrLn "Give a line length : "
     lineLength <- getLine
     let string = concat (drawhorizontalLine (read (lineLength) :: Int))
     putStrLn (string)    

 
 drawhorizontalLine :: Int -> [String]
 drawhorizontalLine 0 = []
 drawhorizontalLine n = (take 1 (repeat ("*"))) ++ drawhorizontalLine (n - 1)  

 designHLineNew :: Int -> Int -> [String]
 designHLineNew totalLine  totalSpace         = (prepareHLineTopBottom totalSpace totalSpace)  ++ (prepareHLineMiddle totalLine totalSpace)  ++ (prepareHLineTopBottom totalSpace totalSpace)

 designHLine :: Int -> Int -> [String]
 designHLine    0       totalSpace         = []
 designHLine totalLine  totalSpace         = (prepareVLine totalSpace totalSpace)  ++ prepareHLineTop (totalLine - 1) totalSpace  

 prepareHLineTopBottom :: Int -> Int -> [String]
 prepareHLineTopBottom  0  starNo = []
 prepareHLineTopBottom line starNo = (take starNo (repeat "*")) ++ ["\n"] ++ prepareHLineTopBottom (line - 1) starNo  

 prepareHLineMiddle :: Int -> Int -> [String]
 prepareHLineMiddle  0  space = []
 prepareHLineMiddle line space = (take space (repeat " ")) ++ ["\n"] ++ prepareHLineMiddle (line - 1) space  

 prepareHLineTop :: Int -> Int -> [String]
 prepareHLineTop line space = (take line (repeat "*")) ++ (take space (repeat "\n")) ++  (take line (repeat "*"))  

 designVLine :: Int -> Int -> [String]
 designVLine    0       totalSpace         = []
 designVLine totalLine  totalSpace         = (prepareVLine totalSpace totalSpace)  ++ designVLine (totalLine - 1) totalSpace  

 prepareVLine :: Int -> Int -> [String]
 prepareVLine line space = (take line (repeat "*")) ++ (take space (repeat " ")) ++  (take line (repeat "*")) ++ ["\n"]  
 --prepareVLine line space = (take 1 (repeat "*")) ++ (take space (repeat " ")) ++  (take 1 (repeat "*")) ++ ["\n"]  


 printTwoVerticalLine :: IO ()
 printTwoVerticalLine = do
     putStrLn "Give a line length : "
     lineLength <- getLine
     let string = concat (designVLine (read (lineLength) :: Int) (read (lineLength) :: Int))
     putStrLn (string)       

 
 printTwoHorizontalLine :: IO ()
 printTwoHorizontalLine = do
     putStrLn "Give a line length : "
     lineLength <- getLine
     let string = concat (designHLineNew (read (lineLength) :: Int) (read (lineLength) :: Int))
     putStrLn (string)       

-----------------------------------------------------------------------------------------------------------------------------------------------------------        
 

 printBox :: IO ()
 printBox = do
     putStrLn "Give a line length : "
     lineLength <- getLine
     let string = concat (designBox (read (lineLength) :: Int))
     putStrLn (string) 


 designBox :: Int -> [String]
 designBox n =    (designBoxTopBottom n) ++ (designBoxLeftRight (n-2) (n-2)) ++ (designBoxTopBottom n) 
 
 designBoxTopBottom :: Int -> [String]
 designBoxTopBottom starNo = (take starNo (repeat " *")) ++ ["\n"]  

 designBoxLeft :: Int -> [String]
 designBoxLeft 0 = []
 designBoxLeft starNo = (take 1 (repeat "!")) ++ ["\n"] ++ designBoxLeft (starNo - 1)  

 designBoxRight :: Int -> [String]
 designBoxRight 0 = []
 designBoxRight starNo = (take starNo (repeat " ")) ++ (take 1 (repeat "!")) ++ ["\n"] ++ designBoxRight (starNo - 1)  

 designBoxLeftRight :: Int -> Int -> [String]
 designBoxLeftRight  0 space     = []
 designBoxLeftRight lineNo space = (take 1 (repeat " *")) ++ (take (space) (repeat "  ")) ++ (take 1 (repeat " *")) ++ ["\n"] ++ designBoxLeftRight (lineNo - 1) space 


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 
 printCross :: IO ()
 printCross = do
     putStrLn "Give a line length : "
     lineLength <- getLine
     let string = concat (designCrossCombine (read (lineLength) :: Int))
     putStrLn (string) 

 designCrossCombine :: Int -> [String]
 designCrossCombine n = (designCrossTop (n `div` 2) 0 (n-2) 0 ) ++ take (n `div` 2) (repeat " ") ++ ["*"]  ++ reverse  (designCrossTop (n `div` 2) 0 (n-2) 0 )  


 designCrossTop :: Int -> Int -> Int -> Int -> [String]
 designCrossTop     0             _                 _                _          = []
 designCrossTop totalLine initialLeftSpace initialMiddleSpace initialRightSpace = designCross initialLeftSpace initialMiddleSpace initialRightSpace ++ 
                                                                                         designCrossTop (totalLine -1) (initialLeftSpace + 1) (initialMiddleSpace -2) (initialRightSpace + 1)    


 designCross :: Int -> Int -> Int -> [String]
 designCross leftSpace middleSpace rightSpace = (take leftSpace (repeat " ")) ++ ["*"] ++ (take middleSpace (repeat " ")) ++ ["*"] ++ (take rightSpace (repeat " ")) ++  ["\n"]   

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

 findMax :: IO ()
 findMax = do
     putStrLn "How many numbers you want to check?"
     n <- getLine
     numbers <- getNumbersList (read n :: Int) 
     let maxValue = maximum (numbers)
     putStrLn ("Max value is " ++ show (maxValue))

 
 getNumbersList :: Int -> IO [Int]
 getNumbersList 0 = return []
 getNumbersList n = do 
     num <- getLine
     let n1 = (read (num) :: Int)
     numbers <- getNumbersList (n-1)
     return (n1:numbers)    



 adder :: IO ()
 adder =  do putStr "How many numbers? "
             n <- getLine
             nums <- getnums (read n :: Int)
             putStrLn ("Their total is: " ++ show (sum nums))

 getnums   :: Int -> IO [Int]
 getnums 0  =  return []
 getnums n   =  do
                 cs <- getLine
                 let num = read cs :: Int
                 nums <- getnums (n-1)
                 return (num:nums)


 getStrings :: Int -> IO [String]
 getStrings 0 = return []
 getStrings n = do 
     str <- getLine
     strings <- getStrings (n-1)
     return (str:strings)


----------------------------------------------------------------------------------------------------------------------------------------------------------------
---------guessGame with built in Random number function
--import System.Random

 guessGame :: IO ()
 guessGame = do
     putStrLn "Please try to guess a number between 1-1000"
     randomMumber <- getLine --raddomRIO (1,1000)
     noOfTries <- playGuessGame 0 0
     --putStrLn "Congratulation. You have done it with " ++ show (noOfTries) ++ "guess...!!!" 
     putStrLn ""

 playGuessGame :: Int -> Int -> IO Int
 playGuessGame secretNumber noOfTry = do
     putStrLn "Guess a number: "
     guessedNumber <- getLine
     let guess =  (read guessedNumber :: Int )
     if (guess == secretNumber) then
         do
         return (noOfTry + 1)
     else if (guess>secretNumber) then
         do
         putStrLn "Too High"
         playGuessGame secretNumber (noOfTry+1) 
     else
         do
         putStrLn "Too Low"
         playGuessGame secretNumber (noOfTry+1)           

     
-------------------------------------------------------------------------------------------------------------------------------------------

---guessGame without built in Random number function. first user initially will give a number and second user will try to guess it correctly

 guessNumberGame :: Int -> IO ()
 guessNumberGame number = do
     putStrLn "Try to guess a number."
     tries <- playGuessNumberGame number 0
     putStrLn ("You needed " ++ show tries ++ " tries to win the game!")
 

 playGuessNumberGame :: Int -> Int -> IO Int 
 playGuessNumberGame secretNumber guessNo = do
     putStrLn "?"
     n <- getLine
     let guess = (read n :: Int) 
     if(guess == secretNumber) then
         do
         return (guessNo + 1)
     else if (guess > secretNumber) then
         do
         putStrLn "Too big"
         playGuessNumberGame secretNumber (guessNo+1)
     else 
         do
         putStrLn "Too small"
         playGuessNumberGame secretNumber (guessNo+1)





 ---guess character. first user initially will give a character and second user will try to guess it correctly

 guessCharacterGame :: Char -> IO ()
 guessCharacterGame character = do
     putStrLn "Try to guess a character."
     tries <- playGuessCharacterGame character 0
     putStrLn ("You needed " ++ show tries ++ " tries to win the game!")


 playGuessCharacterGame :: Char -> Int -> IO Int 
 playGuessCharacterGame secretCharacter guessNo = do
     putStrLn "?"
     guess <- getChar 
     if(guess == secretCharacter) then
         do
         return (guessNo + 1)
     else if (guess > secretCharacter) then
         do
         putStrLn ("Too big " ++ (show guessNo))
         playGuessCharacterGame secretCharacter (guessNo+1)
     else 
         do
         putStrLn ("Too small " ++ (show guessNo))
         playGuessCharacterGame secretCharacter (guessNo+1)

----it's showing output two times.


-----------------------------------------------------------------------------------------------------------------------------------------------------------------
 --   tree 3    tree 4     tree 5

 --     *          *          *
 --    ***        ***        ***
 --   *****      *****      *****
 --     *       *******    *******
 --                *      *********
 --                *          *
 --                           *


     --   diamond 1    diamond 2    diamond 3

     --      *             *             *
     --                   * *           * *
     --                    *           *   *
     --                                 * *
     --                                  *


     --  * *
     --  * *

     --  *   *
     --  *   *
     --  *   *

     --  *     *
     --  *     *
     --  *     *
     --  *     *

     --  *       *
     --  *       *
     --  *       *
     --  *       *
     --  *       *

     --  *         *
     --  *         *
     --  *         *
     --  *         *
     --  *         *
     --  *         *


     --  *           *
     --   *         *
     --    *       *
     --     *     *
     --      *   *
     --       * *
     --        *
     --       * *
     --      *   *
     --     *     *
     --    *       *
     --   *         *
     --  *           *
