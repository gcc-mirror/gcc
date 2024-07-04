! { dg-do run }
!
! PR fortran/103496
!
! Test that C_SIZEOF returns the expected results

program pr103496
  use iso_c_binding
  implicit none
  integer :: a(6)
  integer, pointer :: p(:)

  if (c_sizeof(a)       /= 6*4) stop 1
  if (c_sizeof(a(1))    /=   4) stop 2
  if (c_sizeof(a(:))    /= 6*4) stop 3
  if (c_sizeof(a(2::2)) /= 3*4) stop 4

  allocate(p(5))
  if (c_sizeof(p)       /= 5*4) stop 5
  if (c_sizeof(p(1))    /=   4) stop 6
  if (c_sizeof(p(:))    /= 5*4) stop 7
  if (c_sizeof(p(2::2)) /= 2*4) stop 8
end
