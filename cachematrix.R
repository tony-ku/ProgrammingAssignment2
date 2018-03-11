
##  makeCacheMatrix contains functions that 
##  calculate inverse of a matrix, and will cache
##  if it has been previously calculated
##  It allows the matrix to be set to a new matrix, where
## it would clear the inverse_val

makeCacheMatrix <- function(x = matrix()) {
    inverse_val <- NULL
    set <- function(y) {
        x <<- y
        inverse_val <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse_val <<- solve
    getInverse <- function() inverse_val
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns inverse of a matrix.
## if the inverse of the matrix has already been calculated, do not recalcaulte
## and use the cache value
cacheSolve <- function(x, ...) {
    inverse_val <- x$getInverse()
    if (!is.null(inverse_val)) {
        message ("getting cache data")
        return (inverse_val)
    }
    
    data <- x$get()
    inverse_val <- solve(data, ...)
    x$setInverse(inverse_val)
    inverse_val
}
