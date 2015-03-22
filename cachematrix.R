##There is a pair of functions that cache the inverse of a matrix.

## The first one creates a "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y             
        m <<- NULL
    }
    get <- function() x  
    setinverse <- function(inverse) m <<- inverse  
    getinverse <- function() m
    list(set = set, get = get,    
         setinverse = setinverse,
         getinverse = getinverse)
}

## The second function computes the inverse of the "matrix"
## returned by the first one. If the inverse has already been
## calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {    
        message("getting cached data")
        return(m)        
    }
    data <- x$get()     
    m <- solve(data, ...)  
    x$setinverse(m)
    m
}


