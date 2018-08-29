! { dg-do run { xfail powerpc*-apple-darwin* powerpc*-*-linux* } }
! { dg-require-effective-target fortran_large_real }
! Test XFAILed on these platforms because the system's printf() lacks
! proper support for denormalized long doubles. See PR24685
!
! This tests that the default formats for formatted I/O of reals are
! wide enough and have enough precision, by checking that values can
! be written and read back.
!
include "default_format_2.inc"

program main
  use test_default_format

  if (test (1.0_kl, 0) /= 0) STOP 1
  if (test (0.0_kl, 0) /= 0) STOP 2
  if (test (tiny(0.0_kl), 1) /= 0) STOP 3
  if (test (-tiny(0.0_kl), -1) /= 0) STOP 4
  if (test (huge(0.0_kl), -1) /= 0) STOP 5
  if (test (-huge(0.0_kl), 1) /= 0) STOP 6
end program main
!
