/* Test the vfma_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

#define EPS 1.0e-15

extern void abort (void);

float64_t __attribute__((noipa))
test_vfma (float64x1_t arg1, float64x1_t arg2, float64x1_t arg3)
{
  return vget_lane_f64 (vfma_f64 (arg1, arg2, arg3), 0);
}

int
main (void)
{
  float64_t expected;
  float64_t actual;

  expected = 0.6280448184360076;
  actual = test_vfma (vcreate_f64 (0x3fe3955382d35b0eULL),
		      vcreate_f64 (0x3fa88480812d6670ULL),
		      vcreate_f64 (0x3fd5791ae2a92572ULL));

  if (__builtin_fabs (expected - actual) > EPS)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "fmadd\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 1 } } */
