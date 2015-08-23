## Functions calculate and cache inverse of a matrix

## makeCacheMatrix retuns a list of getter and setter functions for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { ## set matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## get matrix
    setinv <- function(mean) inv <<- mean ## set value for mean 
    getinv <- function() inv ## getting value for inverse matrix 
    ## returned list of functions 
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve checks if the inverse of the matrix is already calculated. if so, returns the cached inverse matrix.
## Otherwise, calculates the inverse matrix and sets the value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get() 
    inv <- solve(data) ## calculate invers matrix
    x$setinv(inv)
    inv
}
