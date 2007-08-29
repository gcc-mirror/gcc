! { dg-do compile }
! Verify that an error is correctly reported if multiple identifiers are given
! with a bind(c) statement that has a NAME= specifier.
module m
  use iso_c_binding
  implicit none
  integer(c_int), bind(C, name="") :: a,b ! { dg-error "Multiple identifiers" }
  integer(c_int), bind(C, name="bob") :: c,d ! { dg-error "Multiple identifiers" }
  integer(c_int) :: e,f 
  bind(c, name="foo") :: e,f ! { dg-error "Multiple identifiers" }
end module m

