## Put comments here that give an overall description of what your
## functions do

## /makeCacheMatrix/ creates a special "matrix" object that caches its
## inverse.
##
## It returns a list of four functions.  The functions are:
## - set: sets the value of the matrix
## - get: gets the value of the matrix
## - setInv: sets the value of the inverse
## - getInv: gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) xInv <<- inv
    getInv <- function() xInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## /cacheSolve/ computes the inverse of the special "matrix returned
## by /makeCacheMatrix/.
##
## If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve retrieves the inverse from the cache and
## returns the result.  Otherwise, it computes the inverse of the
## square matrix and returns that as the result.

cacheSolve <- function(x, ...) {
    xInv <- x$getInv()
    if (!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInv(xInv)
    xInv
}
