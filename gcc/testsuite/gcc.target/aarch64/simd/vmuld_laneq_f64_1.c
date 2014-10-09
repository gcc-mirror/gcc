/* Test the vmuld_laneq_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options " -O3" } */

#include "arm_neon.h"

extern void abort (void);

int
main (void)
{
  volatile float64_t minus_e, pi, ln2;
  float64_t expected, actual;
  float64x2_t arg2;
  float64_t arr[2];

  pi = 3.14159265359;
  arr[0] = minus_e = -2.71828;
  arr[1] = ln2 = 0.69314718056;

  arg2 = vld1q_f64 (arr);
  actual = vmuld_laneq_f64 (pi, arg2, 0);
  expected = pi * minus_e;

  if (expected != actual)
    abort ();

  expected = pi * ln2;
  actual = vmuld_laneq_f64 (pi, arg2, 1);

  if (expected != actual)
    abort ();

  return 0;
}
