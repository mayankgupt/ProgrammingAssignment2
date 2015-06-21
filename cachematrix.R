## makeCacheMatrix function takes a n*n matrix as input and initializes it and 
## also initializes the inverse of the input matrix to NULL and returns the list

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(x) m <<- x
        getInverse <- function() m
        list(setMatrix  = setMatrix , getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve function takes the matrix x and returns the inverse of x for the 
## first time and if the function is called again it returns the already stored cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
