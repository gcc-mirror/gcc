module modull

contains

function fun( a )
  real, intent(in) :: a
  real :: fun
  fun = a
end function fun

end module modull



program t5

use modull

real :: a, b

b = foo( fun, a )

contains

function foo( f, a )
  real, intent(in) :: a
  interface
    function f( x )
      real, intent(in) :: x
      real :: f
    end function f
  end interface
  real :: foo

  foo = f( a )
end function foo

end program t5
