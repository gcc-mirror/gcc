! { dg-do compile }
module x
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none

  common /mycom/ r, s ! { dg-error "does not match" }
  real(c_double) :: r
  real(c_double) :: s
  bind(c, name="my_common_block") :: /mycom/
end module x

module y
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  
  common /mycom/ r, s
  real(c_double) :: r
  real(c_double) :: s
  bind(c, name="my_common_block") :: /mycom/

  common /com2/ i ! { dg-error "does not match" }
  integer(c_int) :: i
  bind(c, name="") /com2/
end module y

module z
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  
  common /mycom/ r, s ! { dg-error "does not match" }
  real(c_double) :: r
  real(c_double) :: s
  ! this next line is an error; if a common block is bind(c), the binding label
  ! for it must match across all scoping units that declare it.
  bind(c, name="my_common_block_2") :: /mycom/ 

  common /com2/ i ! { dg-error "does not match" }
  integer(c_int) :: i
  bind(c, name="mycom2") /com2/
end module z
