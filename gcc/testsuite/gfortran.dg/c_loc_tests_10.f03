! { dg-do compile }
! { dg-options "-std=f2008" }
subroutine aaa(in)
  use iso_c_binding
  implicit none
  integer(KIND=C_int), DIMENSION(:), TARGET  :: in
  type(c_ptr) :: cptr
  cptr = c_loc(in) ! { dg-error "TS 29113: Noninteroperable array at .1. as argument to C_LOC" }
end subroutine aaa
