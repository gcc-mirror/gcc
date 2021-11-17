! { dg-do compile }
! PR fortran/101329 - ICE: Invalid expression in gfc_element_size

program p
  use iso_c_binding
  implicit none
  integer(c_int),     pointer :: ip4
  integer(c_int64_t), pointer :: ip8
  print *, c_sizeof (c_null_ptr) ! valid
  print *, c_sizeof (null ())    ! { dg-error "is not interoperable" }
  print *, c_sizeof (null (ip4)) ! { dg-error "is not interoperable" }
  print *, c_sizeof (null (ip8)) ! { dg-error "is not interoperable" }
end
