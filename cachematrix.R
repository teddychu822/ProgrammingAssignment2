# makeCaheMatrix take a matrix x as argument and return a list containing the following elements
#
#   'set' is the function that initialize the Inversed matrix m
#   'get' is the function that returns the original matrix x
#   'setInverse' stores the inversed matrix m in cache
#   'getInverse' returns the stored Inverse Matrix m
#
makeCacheMatrix <- function (x = matrix()) {
        # m contains the cached version of inversed matrix of x
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inv_x) m <<- Inv_x
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# cacheSolve takes a list created by the makeCacheMatrix function
#   check to see if the inversed of matrix x already exists in cache
cacheSolve <- function(x, ...) {
        # First check to see if a cache version of the Inversed Matrix exists
        m <- x$getInverse()
        if(!is.null(m)) {
                # Cache found. Return the cached version
                message("getting cached data")
                return(m)
        }
        # Cache not found. Retrieve the original matrix
        data <- x$get()
        # Use Solve function to find the inversed matrix
        m <- solve(data, ...)
        # Return and store the inversed matrix in global variable m
        x$setInverse(m)
        m

}
