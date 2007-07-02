! { dg-do compile }
module kind_tests_2
  use, intrinsic :: iso_c_binding

  integer, parameter :: myFKind = c_float
  real(myFKind), bind(c) :: myF
end module kind_tests_2
