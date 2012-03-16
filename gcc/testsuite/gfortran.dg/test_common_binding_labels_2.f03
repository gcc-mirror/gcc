! { dg-do compile }
module test_common_binding_labels_2
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  
  common /mycom/ r, s
  real(c_double) :: r
  real(c_double) :: s
  bind(c, name="my_common_block") :: /mycom/

  common /com2/ i 
  integer(c_int) :: i
  bind(c, name="") /com2/
end module test_common_binding_labels_2
! { dg-final { keep-modules "" } }
