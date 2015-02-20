## Caching the Inverse of Matrix
## This program has two functions ;
##	1. Creates a matrix and caches its inverse. 
##	2. Recalls the inverse matrix from cache. 
##
## Usage: 
##      im <- makeCacheMatrix()
##      im$set(matrix(1:4,2,2))     
##      cacheSolve(im)


## Function to create matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m<<- solve
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Function to retrieve the inverse matrix from cache
cacheSolve <- function(x = matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}