! Test for the ISNAN intrinsic
!
! { dg-do run }
! { dg-add-options ieee }
!
  implicit none
  real :: x
  x = -1.0
  x = sqrt(x)
  if (.not. isnan(x)) STOP 1
  x = 0.0
  x = x / x
  if (.not. isnan(x)) STOP 2

  x = 5.0
  if (isnan(x)) STOP 3
  x = huge(x)
  x = 2*x
  if (isnan(x)) STOP 4
end
