#' @importFrom rstudioapi hasFun getSourceEditorContext setDocumentContents
#' @keywords internal
bracketifyFile <- function(){
  if(hasFun("getSourceEditorContext")){
    editorContext <- getSourceEditorContext()
  }else{
    stop("This version of RStudio is too old!")
  }
  contents <- paste0(editorContext[["contents"]], collapse = "\n")
  newContents <- gsub(
    "(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)*)\\2",
    #"(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)* *)\\2",
    '\\1[["\\3"]]',
    contents, perl=TRUE
  )
  setDocumentContents(newContents, editorContext[["id"]])
}

#' @importFrom rstudioapi hasFun getSourceEditorContext modifyRange
#' @keywords internal
bracketifySelection <- function(){
  if(hasFun("getSourceEditorContext")){
    editorContext <- getSourceEditorContext()
  }else{
    stop("This version of RStudio is too old!")
  }
  selection <- editorContext[["selection"]][[1L]]
  text <- selection[["text"]]
  if(text == ""){
    message("Nothing selected.")
    return(invisible(NULL))
  }
  newText <- gsub(
    "(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)*)\\2",
    #"(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)* *)\\2",
    text, perl=TRUE
  )
  modifyRange(selection[["range"]], newText, editorContext[["id"]])
}

