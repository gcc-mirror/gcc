! { dg-do run { xfail spu-*-* powerpc-ibm-aix* } }
! Test XFAILed on Darwin because the system's printf() lacks
! proper support for denormals.
!
! This tests that the default formats for formatted I/O of reals are
! wide enough and have enough precision, by checking that values can
! be written and read back.
!
include "default_format_1.inc"

program main
  use test_default_format

  if (test (1.0_4, 0) /= 0) call abort
  if (test (tiny(0.0_4), 1) /= 0) call abort
  if (test (-tiny(0.0_4), -1) /= 0) call abort
  if (test (huge(0.0_4), -1) /= 0) call abort
  if (test (-huge(0.0_4), 1) /= 0) call abort

  if (test (1.0_8, 0) /= 0) call abort
  if (test (tiny(0.0_8), 1) /= 0) call abort
  if (test (-tiny(0.0_8), -1) /= 0) call abort
  if (test (huge(0.0_8), -1) /= 0) call abort
  if (test (-huge(0.0_8), 1) /= 0) call abort
end program main
!
