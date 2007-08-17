! { dg-do compile }
module my_kinds
  use, intrinsic :: iso_c_binding
  integer, parameter :: myFKind = c_float
end module my_kinds

module my_module
  use my_kinds
  real(myFKind), bind(c) :: myF  
end module my_module

! { dg-final { cleanup-modules "my_kinds my_module" } }
