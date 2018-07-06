! { dg-do compile }
! PR fortran/85816
! Original code from Martin Diehl <m.diehl at mpie dot de>
!
! Prior to fixing the problem with the array descriptor, gfortran died with
! Operating system error: Cannot allocate memory
! Integer overflow in xmallocarray
!
program test
  implicit none
  real(8) :: tensor(3,3) = 4
  integer :: grid(3) = 16
  ! ok
  write(6,*) spread(spread(tensor,3,grid(1)),4,grid(1))
  ! ok (note the brackets)
  write(6,*) spread((spread(spread(tensor,3,grid(1)),4,grid(2))),5,grid(3))
  ! not ok 
  write(6,*) spread(spread(spread(tensor,3,grid(1)),4,grid(2)),5,grid(3))
end program
