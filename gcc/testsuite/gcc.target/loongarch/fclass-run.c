/* { dg-do run } */
/* { dg-options "-O2 -fsignaling-nans -D_GNU_SOURCE -std=c23" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include "fclass-compile.c"

#define ASSERT_EQ(x, y) (void)(x == y || (__builtin_abort (), 1))

int
main (void)
{
  volatile float f_inf = __builtin_inff ();
  volatile float f_zero = 0;
  volatile float f_normal = 114.514;
  volatile float f_subnormal = 1e-40;
  volatile float f_qnan = __builtin_nanf ("");
  volatile float f_snan = __builtin_nansf ("");
  volatile double d_inf = __builtin_inf ();
  volatile double d_zero = 0;
  volatile double d_normal = 1919.810;
  volatile double d_subnormal = 1e-320;
  volatile double d_qnan = __builtin_nan ("");
  volatile double d_snan = __builtin_nans ("");

#if __loongarch_frlen >= 64
  /* With fclass.{s/d} we shouldn't signal, even if the input is sNaN.
     PR 66462.  */
  feenableexcept (FE_INVALID);
#endif

  ASSERT_EQ (test_fclass_f (f_inf), 0b001);
  ASSERT_EQ (test_fclass_f (-f_inf), 0b001);
  ASSERT_EQ (test_fclass_f (f_zero), 0b100);
  ASSERT_EQ (test_fclass_f (-f_zero), 0b100);
  ASSERT_EQ (test_fclass_f (f_normal), 0b110);
  ASSERT_EQ (test_fclass_f (-f_normal), 0b110);
  ASSERT_EQ (test_fclass_f (f_subnormal), 0b100);
  ASSERT_EQ (test_fclass_f (-f_subnormal), 0b100);
  ASSERT_EQ (test_fclass_f (f_qnan), 0);
  ASSERT_EQ (test_fclass_f (f_snan), 0);

  ASSERT_EQ (test_fclass_d (d_inf), 0b001);
  ASSERT_EQ (test_fclass_d (-d_inf), 0b001);
  ASSERT_EQ (test_fclass_d (d_zero), 0b100);
  ASSERT_EQ (test_fclass_d (-d_zero), 0b100);
  ASSERT_EQ (test_fclass_d (d_normal), 0b110);
  ASSERT_EQ (test_fclass_d (-d_normal), 0b110);
  ASSERT_EQ (test_fclass_d (d_subnormal), 0b100);
  ASSERT_EQ (test_fclass_d (-d_subnormal), 0b100);
  ASSERT_EQ (test_fclass_d (d_qnan), 0);
  ASSERT_EQ (test_fclass_d (d_snan), 0);
}
