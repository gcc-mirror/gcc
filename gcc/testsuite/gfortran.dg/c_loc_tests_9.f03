! { dg-do compile }
subroutine aaa(in)
  use iso_c_binding
  implicit none
  CHARACTER(KIND=C_CHAR), DIMENSION(*), TARGET  :: in
  type(c_ptr) :: cptr
  cptr = c_loc(in)
end subroutine aaa


