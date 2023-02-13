/* Test the vfms_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

#define EPS 1.0e-15

extern void abort (void);

float64_t __attribute__((noipa))
test_vfms (float64x1_t arg1, float64x1_t arg2, float64x1_t arg3)
{
  return vget_lane_f64 (vfms_f64 (arg1, arg2, arg3), 0);
}

int
main (void)
{
  float64_t expected;
  float64_t actual;

  expected = 4.4964705746355915e-2;
  actual = test_vfms (vcreate_f64 (0x3fe730af8db9e6f7ULL),
		      vcreate_f64 (0x3fe6b78680fa29ceULL),
		      vcreate_f64 (0x3feea3cbf921fbe0ULL));

  if (__builtin_fabs (expected - actual) > EPS)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "fmsub\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 1 } } */
