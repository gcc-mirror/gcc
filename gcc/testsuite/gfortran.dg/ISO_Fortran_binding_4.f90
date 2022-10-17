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
    if (str(1) .ne. "bcd") stop 2
    if (str(2) .ne. "ghi") stop 3
    str = ['uvw','xyz']
  end subroutine

  subroutine substr4(str4) BIND(C)
    character(*, kind=4) :: str4(:)
    print *, str4(1)
    print *, str4(2)
    if (str4(1) .ne. 4_"bcd") stop 4
    if (str4(2) .ne. 4_"ghi") stop 5
    str4 = [4_'uvw', 4_'xyz']
  end subroutine

end module

program p
  use mod_ctg
  implicit none
  real :: x(6)
  character(5)         :: str(2)  = ['abcde', 'fghij']
  character(5, kind=4) :: str4(2) = [4_'abcde', 4_'fghij']
  integer :: i

  x = [ (real(i), i=1, size(x)) ]
  call ctg(x(2::2))
  if (any (abs (x - [1.,20.,3.,40.,5.,60.]) > 1.e-6)) stop 3

  !call substr(str(:)(2:4))
  !if (any (str .ne. ['auvwe','fxyzj'])) stop 4

  call substr4(str4(:)(2:4))
  if (any (str4 .ne. [4_'auvwe', 4_'fxyzj'])) stop 4
end program
