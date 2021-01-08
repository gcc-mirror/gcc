! { dg-do compile }
!
! Test the fix for PR98458 in which array expressions within the implied-do
! array constructor caused an ICE in trans-array.c(gfc_conv_array_initializer).
!
! Contributed by Xiao Liu  <xiao.liu@compiler-dev.com>
!
program test
  implicit none
  integer :: i
  integer, parameter :: t(6) = [1,2,3,4,5,6]
  integer, parameter :: tmp(3,2) = reshape([(t(i:i+1),i=1,3)],[3,2])
  print *, tmp  ! Used to ICE
end
