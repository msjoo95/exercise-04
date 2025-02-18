library(tidyverse)

load_dictionary <- function(filename) {
  library(tidyverse)
  words <- readLines(paste0("./data/", filename))
  words <- words[-1]
  return(words)
}

valid_list <- load_dictionary("collins-scrabble-words-2019.txt")
solution_list <- load_dictionary("google-10000-english-usa-no-swears.txt")

str(valid_list)
str(solution_list)

# Step 2
solution_list <- intersect(valid_list,solution_list)
length(solution_list)

# Step 3
pick_solution <- function(list_name, word_length=5){
  filtered <- list_name[nchar(list_name) == word_length]
  filtered <- filtered %>% 
    sample(1, replace = FALSE) %>% 
    strsplit(split = "") %>% 
    unlist()
  return(filtered)
}
solution <- pick_solution(solution_list)
solution

# Step 4
evaluate_guess <- function(guess, solution){
  result <-list()
  for (i in 1:length(guess)) {
    if (guess[i] == solution[i]){
      result[i] <- '*'
    }
    else if ((guess[i] != solution[i]) & (guess[i] %in% solution)){
      result[i] <- '+'
    }
    else if (!guess[i] %in% solution){
      result[i] <- '-'
    }
  }
  return(unlist(result))
}


play_wordle <- function(solution, valid_list, num_guesses=6){
  print("Welcom to the game!")
  print(paste("You have", num_guesses, "chances to guess a word of length", length(solution)))
  print("RULES")
  print("* : in the word and in the correct position")
  print("+ : in the word but in the wrong position")
  print("- : not in the word")
  i <- 1
  letter_diff <- LETTERS
  while (i <= num_guesses) {
    guess <- readline(paste("Attempt", i, "- Enter your guess, then press <enter>: "))
    guess <- toupper(guess)
    guess <- unlist(strsplit(guess, split = ""))
    if (length(guess) != length(solution)){
      print(paste("Length of the word should be", length(solution), "- Try again!"))
      next()
    }
    if (!paste(guess, collapse="") %in% valid_list){
      print("It's not a valid word - Try again!")
      next()
    }
    if (all(guess == solution)){
      print(paste("Congratulations!! You got the answer!!"))
      return()
    }
    
    print(evaluate_guess(guess, solution))
    
    letter_diff <- setdiff(letter_diff, guess)
    print(paste("Letters left:", paste(letter_diff, collapse = ", ")))
    
    i <- i+1
  }
  print(paste("Game over! The answer is:", paste(solution, collapse = "")))
}


play_wordle(solution, valid_list)
