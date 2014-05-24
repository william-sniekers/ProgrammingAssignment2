## This set of functions provides a "datatype" capable of
## * holding a matrix
## * calculating and caching the inverse of this matrix
## * a function to calculate or get the cached inverse of the matrix
##
## Usage:
##  cacheMatrix <- makeCacheMatrix(x)
##      x               is an invertable square matrix
##      cacheMatrix     list will hold the data, optional inverse and functions
##                      to access the matrix
##
##  cacheMatrix$set(x)  set matrix to x
##  cacheMatrix@get()   get matrix
##
##  cacheSolve(cacheMatrix)     will calcultate inverse or get cached result
##
## Heavily inspired om "cached mean" example in assignment 2 Lexical Scoping
## at coursera.org - course "R programming may 2014".


## Constructs a datatype to hold a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {     ## default with empty matrix
        i <- NULL               ## i will cache inverse of x when calculated

        set <- function(y) {
                x <<- y         ## assign (new) vaule
                i <<- NULL      ## matrix changed so assume inverse is obsolete
        }
        get <- function() x                             ## return x when called
        setinverse <- function(inverse) i <<- inverse   ## cache inverse
        getinverse <- function() i                      ## return (cached) inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Reads inverse matrix from cache or calculates and stores inverse if no
## cached inverse is available.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {               ## if cached inverse exists - return it
                return(i)
        }
        matrix <- x$get()               ## calculate inverse and cache it
        i <- solve(matrix, ...)         ## if inverse is not yet cached.
        x$setinverse(i)
        i
}
