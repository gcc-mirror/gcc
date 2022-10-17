! { dg-do compile }
! { dg-additional-options "-Wall" }
!
! Ensure that only those parameters are warned for which are actually used
!
module m
  implicit none
  integer, parameter :: parm = 4   ! unused
  integer, parameter :: parm2 = 4  ! used in the main program
  integer, parameter :: parm3 = 4  ! used in "f()" - { dg-warning "Using parameter 'parm3' declared at .1. is deprecated" }
  integer, save :: var, var2
!GCC$ ATTRIBUTES DEPRECATED :: parm, parm2, parm3, var, var2
contains
  subroutine f()
    print *, parm3 ! warning shown above
  end
end module m

use m  ! { dg-warning "Using parameter 'parm2' declared at .1. is deprecated" }
implicit none
print *, var2, parm2  ! { dg-warning "Using variable 'var2' at .1. is deprecated" }
end
