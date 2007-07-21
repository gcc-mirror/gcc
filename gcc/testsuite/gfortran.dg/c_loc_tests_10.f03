! { dg-do compile }
subroutine aaa(in)
  use iso_c_binding
  implicit none
  integer(KIND=C_int), DIMENSION(:), TARGET  :: in
  type(c_ptr) :: cptr
  cptr = c_loc(in) ! { dg-error "not C interoperable" }
end subroutine aaa
