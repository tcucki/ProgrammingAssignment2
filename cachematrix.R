## Create a cache to a matrix inversion

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    # cache is the inverted matrix cache
    cache <- NULL
    
    getMatrix <- function() {
        x
    }
    setMatrix <- function(newMatrix) {
        x <<- newMatrix
        # invalidates the cache
        cache <<- NULL
    }
    getInv <- function() {
        cache
    }
    setInv <- function(inv) {
        cache <<- inv
    }
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInv = setInv,
         getInv = getInv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {

    # tries to find inverted matrix in the cache
    s <- x$getInv()
    if (!is.null(s)) {
        message("Got data from cache")
        return(s)
    }
    # as the inverted matrix cache was empty, calculates de inverted matrix...
    s <- solve(x$getMatrix())
    # ...and sets the cache
    x$setInv(s)
    s
}
