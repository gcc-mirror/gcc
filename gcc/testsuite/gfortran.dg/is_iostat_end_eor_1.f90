! { dg-do run }
! Test for the Fortran 2003 intrinsics is_iostat_end & is_iostat_eor
!
program test
  use iso_fortran_env
  implicit none
  if ((.not. is_iostat_end(IOSTAT_END)) .or. is_iostat_end(0)) call abort()
  if ((.not. is_iostat_eor(IOSTAT_EOR)) .or. is_iostat_end(0)) call abort()
end program test
