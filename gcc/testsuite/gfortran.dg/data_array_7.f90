! { dg-do run }
!
! Checking for "The new features of Fortran 2008" feature 5.6

  implicit none
  integer :: a(6)
  integer :: b(6)
  integer(kind=4) :: i

  ! Fortran 2008: Subscripts in a data statement can be any constant expression
  data a(kind("foo")) / 1 /
  data a(sum([1, 2, 3]) / 3) / 2 /
  data a(len("foo")) / 3 /
  data a(kind(i)) / 4 /
  data a(int(7.0 * atan(1.0)):6) / 5, 6 /

  ! Fortran 2008: nested implied-do limits in a data statement can be any constant expression
  data (b(i), i = kind("foo"), sum([-1, 1, 2])) / 1, 2 /
  data (b(i), i = len("foo"), kind(i)) / 3, 4 /
  data (b(i), i = int(7.0 * atan(1.0)), 6) / 5, 6 /

  ! Check that data was correctly filled
  if (any(a /= [(i, i = 1, 6)])) stop 1
  if (any(b /= [(i, i = 1, 6)])) stop 1

end
