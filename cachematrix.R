## Put comments here that give an overall description of what your
## functions do

## Creates a 'cached' version of a matrix, whose inverse will be
## stored between requests as long as the matrix is unchanged.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() { x }
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getinv <- function() { inv }
    setinv <- function(i) { inv <<- i }

    list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## Returns a cached matrix's inverse, recomputing it if necessary.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (is.null(i)) {
        xmat <- x$get()
        i <- solve(xmat, ...)
        x$setinv(i)
    } else {
        message("getting cached inverse")
    }
    i
}
