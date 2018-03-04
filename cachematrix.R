## There are two functions that helps calculating the inverse of a matrix, avoiding the
## re-caclutation of a matrix that have been previusly inversed

## Get and set form cache the inverse of a matrix to avoid the long computation of the function solve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    z <- list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
    z$set(x)    #Set the matrix
    z   #return the list with the matrix
}


## Return the inverse of a matrix, checking if it is allready on the cache, to get a faster response

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
