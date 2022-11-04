
#' Question Compiler
#'
#' This function compiles the question, answer list,
#'  and solution from each markdown file into if else statements.
#'
#' @param folder_name Name of folder containing markdowns to compile
#' @return Text output of compiled code
#' @export
qcompile <- function(folder_name){

  setwd(here::here(folder_name))

  file_list <- list.files()
  file_list <- stringr::str_subset(file_list, "q[:digit:]+.Rmd" )
  file_length <- length(file_list)

  print(glue("qver = sample(LETTERS[1:{file_length}],1)"))
  for (i in 1:file_length){
    question = parsermd::parse_rmd(file_list[i])[[2]]

    question_solution <- stringr::str_subset(question, "exsolution:")
    question_solution <- stringr::str_extract_all(question_solution, "[:digit:]+")[[1]]

    question_answer <- suppressWarnings(question[(stringr::str_which(question, "^[:punct:] ")):(stringr::str_which(question, "Solution") - 2)])
    question_answer <- stringr::str_flatten(question_answer, collapse = " ")
    question_answer <- stringr::str_split(question_answer, "  * ")[[1]]
    question_answer <- stringr::str_replace_all(question_answer, "^[:punct:]", "")
    question_answer <- stringr::str_trim(question_answer, side = "both")

    question_title <- question[(stringr::str_which(question, "Question")+3):(stringr::str_which(question, "Answerlist") - 2)]
    question_title <- stringr::str_flatten(question_title, collapse = " ")
    question_title <- stringr::str_replace_all(question_title, "\\\\ ", "\n\n")
    question_title <- stringr::str_replace_all(question_title, "\\\\", "")
    question_title <- stringr::str_replace_all(question_title, "\"", "'")

    q <- c(question_title, list(question_answer), question_solution)

    if (i == 1){
      q_text <- glue::glue("if (qver=='{LETTERS[i]}'){{\n\ttheq = \"{q[[1]][1]}\"\n\tquestions = c(\"{q[[2]][1]}\", \n\t\t\"{q[[2]][2]}\",\n\t\t\"{q[[2]][3]}\",\n\t\t\"{q[[2]][4]}\")\n\tsolutions = \"{q[[3]][1]}\"")
    } else if (i == file_length){
      q_text <- glue::glue("}}else if(qver=='{LETTERS[i]}'){{\n\ttheq = \"{q[[1]][1]}\"\n\tquestions = c(\"{q[[2]][1]}\", \n\t\t\"{q[[2]][2]}\",\n\t\t\"{q[[2]][3]}\",\n\t\t\"{q[[2]][4]}\")\n\tsolutions = \"{q[[3]][1]}\"\n}}")
    } else{
      q_text <- glue::glue("}}else if(qver=='{LETTERS[i]}'){{\n\ttheq = \"{q[[1]][1]}\"\n\tquestions = c(\"{q[[2]][1]}\", \n\t\t\"{q[[2]][2]}\",\n\t\t\"{q[[2]][3]}\",\n\t\t\"{q[[2]][4]}\")\n\tsolutions = \"{q[[3]][1]}\"")
    }
    print(q_text)
  }
}

