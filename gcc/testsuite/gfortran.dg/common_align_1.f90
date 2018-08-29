! { dg-do run }
! { dg-options "-fno-align-commons" }

! PR fortran/37486
!
! Test for -fno-align-commons.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>.

subroutine one()
  integer :: i
  common i
  if (i/=5) STOP 1
end subroutine one

program test
integer :: i
real(8) :: r8
common i, r8 
i = 5
call one()
end program test
