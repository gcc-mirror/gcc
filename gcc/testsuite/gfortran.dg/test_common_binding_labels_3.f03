! { dg-do compile }
module test_common_binding_labels_3
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none
  
  common /mycom/ r, s
  real(c_double) :: r
  real(c_double) :: s
  bind(c, name="my_common_block") :: /mycom/
end module test_common_binding_labels_3
! { dg-final { keep-modules "" } }
