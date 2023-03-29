/* Test the vmul_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

float64_t __attribute__((noipa))
test_vmul (float64x1_t arg1, float64x1_t arg2)
{
  return vget_lane_f64 (vmul_f64 (arg1, arg2), 0);
}

int
main (void)
{
  float64_t minus_e, pi;
  float64_t expected, actual;

  pi = 3.14159265359;
  minus_e = -2.71828;

  expected = pi * minus_e;
  actual = test_vmul ((float64x1_t) { pi }, (float64x1_t) { minus_e });
  if (expected != actual)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler "fmul\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" } } */
