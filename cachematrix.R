## The makeCacheMatrix function creates a special matrix which is
## then used in cacheSolve to check whether the inverse has already
## been calculated or not

## Creates a special matrix which is basically a list of 4 functions

makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv<-inverse
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Uses the list created above to check whether the inverse has already been computed
## Hence returns inverse either from cache or new calculations

cacheSolve <- function(x, ...) 
{
  inv<-x$getinv()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
