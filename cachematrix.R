## this pair of functions demonstrate R code 

## you must call the first function makeCacheMatrix()
## to set up the data structures to cache a matrix inverse
## then call cacheSolve() to actually compute the inverse
## using the function handle returned by makeCacheMatrix()

## calling makeCacheMatrix(x) creates a function handle for
## matrix "x" that can later be used to compute and retrieve
## its inverse matrix using member functions
makeCacheMatrix <- function(x = matrix()) {
    ## called first, initialize stored inverse to NULL
    m <- NULL

    ## sets stored matrix to new value "y" and deletes stored inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## retrieves the original matrix from the function handle
    get <- function() x

    ## sets the inverse matrix to a value computed elsewhere
    setinv <- function(solved) m <<- solved

    ## retrieves the inverse matrix or NULL if not computed yet
    getinv <- function() m

    ## list the available operations of handle
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
 
 
## calling cacheSolve(h) for function handle "h" previously
## returned by makeCacheMatrix() will provide the _alleged_ 
## inverse matrix by retrieving stored value if available
## or otherwise computing it
cacheSolve <- function(x, ...) {

    ## if inverse matrix previously cached, print message
    ## and return cached value
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## otherwise get original matrix from handle and use it
    ## to calculate inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
