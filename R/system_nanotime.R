#' Return CPU time that \sQuote{expr} used with sub-milliseconds accuracy.
#'
#' Clone of base R \code{system.time} function utilizing accuracy of \code{microbenchmark} package.
#' 
#' @param expr Valid R expression to be timed.
#' @param gcFirst Logical - should a garbage collection be performed immediately before the timing? Default is TRUE.
#'
#' @note 
#'  The sole purpose of this function is to have a drop-in replacement for \code{system.time}.
#'  Due to the implementation this function overestimates the runtime of \code{expr}. 
#'  For more accurate timing, please use \code{\link{microbenchmark}}.
#'
#' @return A \code{proc_time} class object, only the \code{elapsed} value is returned, all others are \code{NA}.
#' 
#' @seealso \code{\link{get_nanotime}}
#' @export
#' @author Jan Gorecki and Olaf Mersmann
#' @examples
#' system.time(replicate(10, mad(runif(100))))
#' system.nanotime(replicate(10, mad(runif(100))))
#' print(microbenchmark(replicate(10, mad(runif(100)))), unit="s")
system.nanotime = function(expr, gcFirst = TRUE){
    if (gcFirst) gc(FALSE)

    ## Try to estimate overhead of surrounding code.
    overhead <- median(replicate(100, {
        overhead_time <- get_nanotime()
        on.exit(cat("woops"))
        new.overhead_time <- get_nanotime()
        on.exit()
        new.overhead_time - overhead_time
    }))

    ## Actual timing
    time <- get_nanotime()
    on.exit(cat("Timing stopped at:", (get_nanotime() - time) * 1e-9, "\n"))
    expr
    new.time <- get_nanotime()
    on.exit()
    elapsed <- (new.time - time - overhead) * 1e-9
    structure(c(NA_real_, NA_real_, elapsed, NA_real_, NA_real_),
              .Names=c("user.self", "sys.self", "elapsed", "user.child", "sys.child"),
              class = "proc_time")
}
