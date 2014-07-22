/* Test the vbsl_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

extern void abort (void);

int
main (void)
{
  float64x1_t expected, actual;
  float64_t expected_scalar, actual_scalar;
  float64x1_t arg1, arg2;
  uint64_t mask = 0xf0fc00fbf000fa0fULL;
  uint64_t arg1_uint = 0xdeadbeefbada9832ULL;
  uint64_t arg2_uint = 0xcafe3254deed7111ULL;

  arg1 = vcreate_f64 (arg1_uint);
  arg2 = vcreate_f64 (arg2_uint);
  expected = vcreate_f64 ((arg1_uint & mask) | (arg2_uint & ~mask));
  actual = vbsl_f64 (vcreate_u64 (mask), arg1, arg2);

  expected_scalar = vget_lane_f64 (expected, 0);
  actual_scalar = vget_lane_f64 (actual, 0);

  if (expected_scalar != actual_scalar)
    abort ();

  return 0;
}
