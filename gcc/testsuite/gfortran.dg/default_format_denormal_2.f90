! { dg-do run { xfail { long_double_is_ibm128 || hppa*-*-hpux* } } }
! { dg-require-effective-target fortran_large_real }
! Test XFAILed on this platform because the system's printf() lacks
! proper support for denormalized long doubles. See PR24685
! On hppa*-*-hpux*, snprintf only provides 33 digits of decimal
! precision for long doubles.  This is insufficient to uniquely
! identify long double values.
!
! This tests that the default formats for formatted I/O of reals are
! wide enough and have enough precision, by checking that values can
! be written and read back.
!
! { dg-add-options ieee }

include "default_format_2.inc"

program main
  use test_default_format

  if (test (tiny(0.0_kl), -1) /= 0) STOP 1
  if (test (-tiny(0.0_kl), 1) /= 0) STOP 2
end program main
!
