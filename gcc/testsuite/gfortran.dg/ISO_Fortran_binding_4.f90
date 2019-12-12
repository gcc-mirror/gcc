! { dg-do  run }
! PR fortran/89384 - this used to give a wrong results
! with contiguous.
! The subroutine substr is a test to check a problem found while
! debugging PR90355.
!
! Test case by Reinhold Bader.
!
module mod_ctg
  implicit none

contains

  subroutine ctg(x) BIND(C)
    real, contiguous :: x(:)
    if (any(abs(x - [2.,4.,6.]) > 1.e-6)) stop 1
    x = [2.,4.,6.]*10.0
  end subroutine

  subroutine substr(str) BIND(C)
    character(*) :: str(:)
    if (str(2) .ne. "ghi") stop 2
    str = ['uvw','xyz']
  end subroutine

end module

program p
  use mod_ctg
  implicit none
  real :: x(6)
  character(5) :: str(2) = ['abcde','fghij']
  integer :: i

  x = [ (real(i), i=1, size(x)) ]
  call ctg(x(2::2))
  if (any (abs (x - [1.,20.,3.,40.,5.,60.]) > 1.e-6)) stop 3

  call substr(str(:)(2:4))
  if (any (str .ne. ['auvwe','fxyzj'])) stop 4
end program
