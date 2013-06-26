! { dg-do compile }
! { dg-options "-std=f2003" }
!
module x
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none

  common /mycom/ r, s ! { dg-error "In Fortran 2003 COMMON 'mycom' block at .1. is a global identifier and must thus have the same binding name as the same-named COMMON block at .2.: my_common_block vs .blank.|In Fortran 2003 COMMON 'mycom' block at .1. is a global identifier and must thus have the same binding name as the same-named COMMON block at .2.: my_common_block_2 vs .blank." }
  real(c_double) :: r
  real(c_double) :: s
  bind(c, name="my_common_block") :: /mycom/
end module x

module y
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  
  common /mycom/ r, s ! { dg-error "In Fortran 2003 COMMON 'mycom' block at .1. is a global identifier and must thus have the same binding name as the same-named COMMON block at .2.: my_common_block vs .blank." }
  real(c_double) :: r
  real(c_double) :: s
  bind(c, name="my_common_block") :: /mycom/

  common /com2/ i ! { dg-error " In Fortran 2003 COMMON 'com2' block at .1. is a global identifier and must thus have the same binding name as the same-named COMMON block at .2.: mycom2 vs .blank." }
  integer(c_int) :: i
  bind(c, name="") /com2/
end module y

module z
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  
  common /mycom/ r, s ! { dg-error "In Fortran 2003 COMMON 'mycom' block at .1. is a global identifier and must thus have the same binding name as the same-named COMMON block at .2.: my_common_block_2 vs .blank." }
  real(c_double) :: r
  real(c_double) :: s
  ! this next line is an error; if a common block is bind(c), the binding label
  ! for it must match across all scoping units that declare it.
  bind(c, name="my_common_block_2") :: /mycom/ 

  common /com2/ i ! { dg-error " In Fortran 2003 COMMON 'com2' block at .1. is a global identifier and must thus have the same binding name as the same-named COMMON block at .2.: mycom2 vs .blank." }
  integer(c_int) :: i
  bind(c, name="mycom2") /com2/
end module z
