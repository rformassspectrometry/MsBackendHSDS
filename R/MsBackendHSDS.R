#' @include hidden_aliases.R
NULL

#' @title MS data backend for HDF5 files served from an HDF5 server
#'
#' @aliases MsBackendHSDS-class
#'
#' @description
#'
#'
#' @param object Instance of `MsBackendHmdbXml` class.
#'
#' @param files `character` with the (full) file name(s) of the HMDB xml file(s)
#'     from which MS/MS data should be imported.
#'
#' @param nonStop `logical(1)` whether import should be stopped if an xml file
#'     does not contain all required fields. Defaults to `nonStop = FALSE`.
#'
#' @param BPPARAM Parameter object defining the parallel processing setup to
#'     import data in parallel. Defaults to `BPPARAM = bpparam()`. See
#'     [bpparam()] for more information.
#'
#' @param ... Currently ignored.
#'
#' @author Johannes Rainer
#'
#' @importClassesFrom Spectra MsBackendDataFrame
#'
#' @exportClass MsBackendHmdbXml
#'
#' @name MsBackendHmdbXml
#'
#' @examples
#'
#' ## Create an MsBackendHmdbXml backend and import data from test xml files.
#' fls <- dir(system.file("xml", package = "MsBackendHmdb"),
#'     full.names = TRUE, pattern = "xml$")
#' be <- backendInitialize(MsBackendHmdbXml(), fls)
#' be
#'
#' be$msLevel
#' be$compound_id
NULL

setClass("MsBackendHmdbXml",
         contains = "MsBackendDataFrame",
         prototype = prototype(spectraData = DataFrame(),
                               readonly = FALSE,
                               version = "0.1"))

#' @importMethodsFrom Spectra backendInitialize spectraData<- $<- $
#'
#' @importFrom BiocParallel bpparam
#'
#' @importMethodsFrom BiocParallel bplapply
#'
#' @importFrom methods validObject
#'
#' @exportMethod backendInitialize
#'
#' @rdname MsBackendHmdbXml
setMethod("backendInitialize", signature = "MsBackendHSDS",
          function(object, files, nonStop = FALSE, ..., BPPARAM = bpparam()) {
              if (missing(files) || !length(files))
                  stop("Parameter 'files' is mandatory for ", class(object))
              if (!is.character(files))
                  stop("Parameter 'files' is expected to be a character vector",
                       " with the files names from where data should be",
                       " imported")
              files <- normalizePath(files)
              if (any(!file.exists(files)))
                  stop("file(s) ",
                       paste(files[!file.exists(files)], collapse = ", "),
                       " not found")
              ## Import data and rbind.
              message("Start data import from ", length(files), " files ... ",
                      appendLF = FALSE)
              res <- bplapply(files, FUN = .import_hmdb_ms_ms_spectrum,
                              nonStop = nonStop, BPPARAM = BPPARAM)
              message("done")
              res <- do.call(rbind, res)
              if (nonStop && length(files) > nrow(res))
                      warning("Import failed for ", length(files) - nrow(res),
                              " files")
              spectraData(object) <- res
              object$dataStorage <- "<memory>"
              object$centroided <- TRUE
              validObject(object)
              object
          })

#' @rdname MsBackendHmdbXml
#'
#' @importFrom methods new
#'
#' @export MsBackendHmdbXml
MsBackendHmdbXml <- function() {
    new("MsBackendHmdbXml")
}
