! { dg-do compile }
! { dg-options "-std=f2003" }
subroutine aaa(in)
  use iso_c_binding
  implicit none
  integer(KIND=C_int), DIMENSION(:), TARGET  :: in
  type(c_ptr) :: cptr
  cptr = c_loc(in) ! { dg-error "Fortran 2008: Array of interoperable type at .1. to C_LOC which is nonallocatable and neither assumed size nor explicit size" }
end subroutine aaa
