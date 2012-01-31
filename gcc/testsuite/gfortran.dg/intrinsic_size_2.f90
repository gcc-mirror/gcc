! { dg-do compile }
!
! PR fortran/51904
!
! Contributed by David Sagan.
!

call qp_draw_polyline_basic([1.0,2.0])
contains
subroutine qp_draw_polyline_basic (x)
  implicit none
  real :: x(:), f
  integer :: i
  f = 0
  print *, size(f*x)
end subroutine
end
