! { dg-do compile }
! Tests the fix for PR29821, which was due to failure to simplify the
! array section, since the section is not constant, provoking failure
! to resolve the argument of SUM and therefore to resolve SUM itself.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de>
!
module gfcbug45
  implicit none
contains
  subroutine foo 
    real, external :: mysum
    integer :: i
    real    :: a
    real, parameter :: eps(2) = (/ 1, 99 /)
    i = 1
    a = sum (eps(i:i+1) * eps)
    print *, a
  end subroutine foo
end module gfcbug45
  use gfcbug45
  call foo
end
! { dg-final { cleanup-modules "gfcbug45" } }
