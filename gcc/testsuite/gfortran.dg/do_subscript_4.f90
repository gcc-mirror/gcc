! { dg-do compile }
! PR 91424 - this used to warn although the DO loop is zero trip.
program main
  implicit none
  integer :: i
  real :: a(2)
  do i=1,3,-1
     a(i) = 2.
  end do
  print *,a
end program main
