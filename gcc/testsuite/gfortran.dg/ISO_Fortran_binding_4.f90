! { dg-do  run }
! PR fortran/89384 - this used to give a wrong results
! with contiguous.
! Test case by Reinhold Bader.
module mod_ctg
  implicit none
contains
  subroutine ctg(x) BIND(C)
    real, contiguous :: x(:)

    if (any(abs(x - [2.,4.,6.]) > 1.e-6)) then
       write(*,*) 'FAIL'
       stop 1
    else
       write(*,*) 'OK'
    end if
    x = [2.,4.,6.]*10.0
  end subroutine
end module
program p
  use mod_ctg
  implicit none
  real :: x(6)
  integer :: i

  x = [ (real(i), i=1, size(x)) ]
  call ctg(x(2::2))
  if (any (abs (x - [1.,20.,3.,40.,5.,60.]) > 1.e-6)) stop 2
end program
