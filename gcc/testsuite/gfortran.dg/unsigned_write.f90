! { dg-do  run }
! This is a libgfortran (runtime library) test, need to run only once!
!
! { dg-require-effective-target fortran_integer_16 }
! { dg-additional-options "-funsigned" }
!
! PR libfortran/118406 - printing large UNSIGNED(kind=16) crashes

program print_large_unsigned
  unsigned(16), parameter :: u16_max =      huge(0U_16)
  unsigned(16), parameter :: u8_max  = uint(huge(0U_8),16)        ! UINT64_MAX
  unsigned(16), parameter :: ten19   = uint(10_8 ** 18,16)*10U_16 ! 10**19
  character(42) :: s

  ! Reference: signed integer
  write(s,*)       huge(0_16)
  if (adjustl (s) /= "170141183460469231731687303715884105727") stop 1

  ! Same value as unsigned
  write(s,*) uint (huge(0_16),16)
  if (adjustl (s) /= "170141183460469231731687303715884105727") stop 2

  ! Extreme and intermediate values
  write(s,*) u16_max
  if (adjustl (s) /= "340282366920938463463374607431768211455") stop 3

  write(s,*) (u16_max - 3U_16) / 4U_16 * 3U_16
  if (adjustl (s) /= "255211775190703847597530955573826158589") stop 4

  ! Test branches of implementation in string.c::gfc_itoa
  write(s,*) u8_max * ten19
  if (adjustl (s) /= "184467440737095516150000000000000000000") stop 5

  write(s,*) u8_max * ten19 - 1U_16
  if (adjustl (s) /= "184467440737095516149999999999999999999") stop 6

  write(s,*) u8_max * ten19 + 1U_16
  if (adjustl (s) /= "184467440737095516150000000000000000001") stop 7

end
