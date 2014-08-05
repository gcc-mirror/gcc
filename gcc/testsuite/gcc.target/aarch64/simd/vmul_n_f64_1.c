/* Test the vmul_n_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

extern void abort (void);

int
main (void)
{
  volatile float64_t minus_e, pi;
  float64_t expected, actual;

  pi = 3.14159265359;
  minus_e = -2.71828;

  expected = pi * minus_e;

  actual = vget_lane_f64 (vmul_n_f64 ((float64x1_t) { pi },
                                       minus_e), 0);
  if (expected != actual)
    abort ();

  return 0;
}
