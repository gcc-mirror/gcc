! { dg-do run }
! Tests the fix for PR42104 in which the call to the procedure pointer
! component caused an ICE because the "always_implicit flag was not used
! to force the passing of a descriptor for the array argument.
!
! Contributed by Martien Hulsen <m.a.hulsen@tue.nl>
!
module poisson_functions_m

  implicit none

contains
 
  function func ( nr, x )
    integer, intent(in) :: nr
    real, intent(in), dimension(:) :: x
    real :: func

    real :: pi 

    pi = 4 * atan(1.)

    select case(nr)
      case(1)
        func = 0
      case(2)
        func = 1
      case(3)
        func = 1 + cos(pi*x(1))*cos(pi*x(2))
      case default
        write(*,'(/a,i0/)') 'Error func: wrong function number: ', nr
        stop
    end select

  end function func 

end module poisson_functions_m
 
module element_defs_m

  implicit none

  abstract interface 
    function dummyfunc ( nr, x )
      integer, intent(in) :: nr
      real, intent(in), dimension(:) :: x
      real :: dummyfunc
    end function dummyfunc
  end interface 

  type function_p
    procedure(dummyfunc), nopass, pointer :: p => null()
  end type function_p

end module element_defs_m

program t

use poisson_functions_m
use element_defs_m

procedure(dummyfunc), pointer :: p => null()
type(function_p) :: funcp

p => func
funcp%p => func

print *, func(nr=3,x=(/0.1,0.1/))
print *, p(nr=3,x=(/0.1,0.1/))
print *, funcp%p(nr=3,x=(/0.1,0.1/))

end program t
! { dg-final { cleanup-modules "poisson_functions_m element_defs_m" } }
