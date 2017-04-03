! Testcase for using EQUIVALENCE with BIND(C)
! See PR fortran/39239
! { dg-do compile }
module m
  use iso_c_binding
  implicit none
  integer(c_int) :: i1, i2
  bind(C) :: i2 
  equivalence(i1,i2) ! { dg-error "EQUIVALENCE attribute conflicts with BIND" }
end module m

