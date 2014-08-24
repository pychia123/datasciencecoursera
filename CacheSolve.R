


## Caches the matrix object and store the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve 
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Returns the inverse matrix from the cache if the inverse matrix of x was computed before. 
## Otherwise, computes the inverse matrix value for the given matrix x 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

         m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
		print ("cached")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}