## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse; basis is the "makeVector" example
## from the instructions page. In this function "inv" and "inverse" are replacing "m" and "mean", respectively.
## For a higher readability, "inverse" is separated by an underscore from its set and get.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                set_inverse <- function(inverse) inv <<- inverse
                get_inverse <- function() inv
                list(set = set, 
                     get = get,
                     set_inverse = set_inverse,
                     get_inverse = get_inverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## This function is now computing the inverse of "makeCacheMatrix"(). If the inverse of the matrix has been already computed, it will be called from the cache (given that the matrix hasn't been changed).
        ## Here again, the example from the instruction page is the basis ("cachemean"). "m" and "mean" is again
        ## replaced by "inv" and "inverse". Note, that in line 42 (the "Answer to the Ultimate Question of Life, 
        ## the Universe, and Everything") "mean" was replaced by the funcion "solve".
        ## As in "makeCacheMatrix", for a higher readability, "inverse" is
        ## separated by an underscore from its "set" and "get".
                inv <- x$get_inverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$set_inverse(inv)
                inv
}

## The following is the way I tested both functions; please try this approach as well as your own to stress-test
## both functions.
## To test these both functions, create an invertible matrix by calling the "makeCacheMatrix" function, e.g.
## x <- makeCacheMatrix(matrix(1:4, 2, 2))
## and check the result by "x$get()".
## Solve the small matrix with the "cacheSolve" function:
## cacheSolve(x)
## Check the result with the "get_inverse":
## x$get_inverse()
