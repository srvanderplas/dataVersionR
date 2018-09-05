#' Hash one or more values (non-vectorized)
#'
#' @param ... values
#' @return a hash of the provided values
#' @example hash_scalar(rnorm(100))
#' @importFrom digest digest
hash_scalar <- function(...) {
  x <- list(...)

  digest::digest(x)
}

#' A semi-vectorized hash function
#'
#' If the ... arguments all have the same length, this function will return
#' a vector of the same length with hashes corresponding to each entry in the
#' corresponding list. If the ... arguments don't have the same length,
#' the overall hash of all values is returned.
#' @param ... values
#' @return a vector of hashed values (if ... all have the same length), a scalar otherwise
#' @export
#' @importFrom purrr pmap_chr
hash_precursor <- function(...) {
  x <- list(...)
  x_len <- lapply(x, length) %>% unlist()
  if (min(x_len) > 1 & length(unique(x_len)) == 1) {
    # If all ... args have the same length, use pmap:
    purrr::pmap_chr(x, hash_scalar)
  } else {
    hash_scalar(...)
  }
}

#' Determine if the hashed value has changed
#'
#' Assumes that the path provided is the path where the data is stored.
#' @param data_file_path path where the data is stored. ".hash" is appended
#'          to the file name, and if that file exists, the lines are read and
#'          compared to the value of the hashed object
#' @param ... values to hash - unnecessary if precursor_hash is not null
#' @param precursor_hash hash of values to compare to hash saved in data_file_path.hash
#' @export
#' @importFrom stringr str_trim
precursor_hash_changed <- function(data_file_path, ..., precursor_hash = NULL) {
  if (!file.exists(paste0(data_file_path, ".hash"))) {
    precursor_hash_prev <- ""
  } else {
    precursor_hash_prev <- readLines(paste0(data_file_path, ".hash"))
  }

  if (is.null(precursor_hash)) {
    stopifnot(length(list(...)) > 0)
    precursor_hash = hash_precursor(...)
  }

  compare <- stringr::str_trim(precursor_hash) ==
    stringr::str_trim(precursor_hash_prev)

  return(!compare)
}

#' Write hashed value to file
#'
#' Assumes that the path provided is the path where the data created using the
#' precursor value is stored.
#' @param data_file_path path where the data is stored. ".hash" is appended
#'          to the file name, and if that file exists, the lines are read and
#'          compared to the value of the hashed object
#' @param ... values to hash - unnecessary if precursor_hash is not null
#' @param precursor_hash hash of values to compare to hash saved in data_file_path.hash
#' @export
precursor_hash_write <- function(data_file_path, ..., precursor_hash = NULL) {
  if (is.null(precursor_hash)) {
    stopifnot(length(list(...)) > 0)
    precursor_hash = hash_precursor(...)
  }

  writeLines(precursor_hash, con = paste0(data_file_path, ".hash"))
}