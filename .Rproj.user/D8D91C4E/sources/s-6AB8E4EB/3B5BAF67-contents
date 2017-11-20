#' @title Producing PCA greatest hits
#'
#' @description Produce a complete table of PCA loadings,
#' and a biplot of the first two principal components, if the table
#' is less than 101 observations and 16 columns.
#'
#' @param x a matrix or dataframe object
#'
#' @examples
#' one <- cbind(rnorm(50, 1, 1), rnorm(50, 0, 1), rnorm(50, 3, 1.5))
#' pca_time(one)
#'
#'
#' @export


pca_time <- function(x) {

   # we need the input here to be a matrix or data frame
   if(!is.matrix(x) & !is.data.frame(x) ){
     stop("Input must be a matrix or data frame")
   }

   pca_hold <- stats::princomp(x, cor=TRUE)

   # get a quick biplot
   if(nrow(x) <= 100 & ncol(x) <= 15) {
   plot_hold <- stats::biplot(pca_hold
          , main="Biplot of First Two Principal Components"
          )
   return(list(pca_hold$loadings, plot_hold))
   }

   else {
   return(pca_hold$loadings)
   }
 }

