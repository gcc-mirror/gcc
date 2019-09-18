! { dg-do run { xfail *-*-darwin[89]* *-*-cygwin* } }
! Test XFAILed on these platforms because the system's printf() lacks
! proper support for denormals.
!
! This tests that the default formats for formatted I/O of reals are
! wide enough and have enough precision, by checking that values can
! be written and read back.
!
! { dg-add-options ieee }

include "default_format_1.inc"

program main
  use test_default_format

  if (test (tiny(0.0_4), -1) /= 0) STOP 1
  if (test (-tiny(0.0_4), 1) /= 0) STOP 2
  if (test (0.0_4, 0) /= 0) STOP 3

  if (test (tiny(0.0_8), -1) /= 0) STOP 4
  if (test (-tiny(0.0_8), 1) /= 0) STOP 5
  if (test (0.0_8, 0) /= 0) STOP 6

end program main
!
