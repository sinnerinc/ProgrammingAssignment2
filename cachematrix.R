# This file contains functions allowing to calculate and store inverse of a
# matrix. I sticked to what was presented in order to be consistent with example
# provided.

# Function for storing a matrix and its inverse. Also allows internal matrix
# to be changed once we desire so.
makeCacheMatrix <- function(x = matrix()) {

    # This stores our inverse matrix. Initially let it be NULL
    inv <- NULL

    ## This setter allows to change value currently being hold. If we change
    ## value of stored matrix, we need to reset its inverse as well
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## Return our original matrix (it was passed as parameter)
    get <- function() { x }

    ## Set inverse of our matrix. Use superassignment, so we actually save
    ## this to our makeCacheMatrix$inv variable
    setinverse <- function(inverse) { inv <<- inverse }

    ## Get our inverse matrix
    getinverse <- function() { inv }

    ## This is our "return value" - which in fact is list of functions our
    ## "object" contains, so that we can get and set new matrix and its
    ## inverse as well
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Function returns inverse of a given matrix. It firstly however checks
# whether we already have it stored in 'cache', and if so return this value.
# If not, calculate inverse and save it.
cacheSolve <- function(x) {

    ## Get matrix that is the inverse of 'x'
    inv <- x$getinverse()

    ## Check if what we got is inverse or actually a NULL. If inverse, return
    ## that and indicate we're getting cached data.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Since we're here, we don't have inverse. So let's get original matrix...
    originalMatrix <- x$get()

    ## ...and calculate its inverse...
    inv <- solve(originalMatrix)

    ## ...and finally cache it in our "special object"
    x$setinverse(inv)

    ## Return the inverse
    inv
}
