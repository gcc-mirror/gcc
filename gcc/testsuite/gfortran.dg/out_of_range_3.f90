! { dg-do run }
! { dg-require-effective-target fortran_integer_16 }
! { dg-additional-options "-funsigned" }
!
! PR fortran/115788 - OUT_OF_RANGE

program p
  use iso_fortran_env, only: real32, real64
  implicit none
  unsigned(16) :: u16
  real(real32) :: r
  real(real64) :: d

  u16 = huge(0U_16)
  if (.not. OUT_OF_RANGE (u16        ,r)) stop 1
  if (.not. OUT_OF_RANGE (huge(0U_16),r)) stop 2
  if (      OUT_OF_RANGE (u16        ,d)) stop 3
  if (      OUT_OF_RANGE (huge(0U_16),d)) stop 4

  ! This still fits into a 32-bit IEEE float
  u16 = huge(0U_16)/65536U_16*65535U_16
  if (      OUT_OF_RANGE (u16                            ,r)) stop 5
  if (      OUT_OF_RANGE (huge(0U_16)/65536U_16*65535U_16,r)) stop 6

end
