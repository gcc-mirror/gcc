! { dg-do compile }
! PR fortran/18271
subroutine sub(imax)
  implicit none
  integer, intent(in) :: imax
  real :: aux1(25000+int(0.82*imax))
end subroutine
