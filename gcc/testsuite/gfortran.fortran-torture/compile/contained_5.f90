! Function returning an array continaed in a module.  Caused problems 'cos
! we tried to add the dummy return vars to the parent scope.

Module contained_5
contains
FUNCTION test ()
  REAL, DIMENSION (1) :: test
  test(1)=0.0
END FUNCTION 
end module
