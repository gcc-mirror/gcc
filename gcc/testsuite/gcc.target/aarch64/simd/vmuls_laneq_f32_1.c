/* Test the vmuls_laneq_f32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options " -O3" } */

#include "arm_neon.h"

extern void abort (void);

int
main (void)
{
  volatile float32_t minus_e, pi, ln2, sqrt2, phi;
  float32_t expected, actual;
  float32x4_t arg2;
  float32_t arr[4];

  pi = 3.14159265359;
  arr[0] = minus_e = -2.71828;
  arr[1] = ln2 = 0.69314718056;
  arr[2] = sqrt2 = 1.41421356237;
  arr[3] = phi = 1.61803398874;

  arg2 = vld1q_f32 (arr);
  actual = vmuls_laneq_f32 (pi, arg2, 0);
  expected = pi * minus_e;

  if (expected != actual)
    abort ();

  expected = pi * ln2;
  actual = vmuls_laneq_f32 (pi, arg2, 1);

  if (expected != actual)
    abort ();

  expected = pi * sqrt2;
  actual = vmuls_laneq_f32 (pi, arg2, 2);

  if (expected != actual)
    abort ();

  expected = pi * phi;
  actual = vmuls_laneq_f32 (pi, arg2, 3);

  if (expected != actual)
    abort ();

  return 0;
}
