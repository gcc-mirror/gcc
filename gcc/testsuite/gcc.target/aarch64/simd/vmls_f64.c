/* Test the vmls_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

#define EPS 1.0e-15

extern void abort (void);

int
main (void)
{
  float64x1_t arg1;
  float64x1_t arg2;
  float64x1_t arg3;

  float64_t expected;
  float64_t actual;

  arg1 = vcreate_f64 (0x3fea7ec860271ad9ULL);
  arg2 = vcreate_f64 (0x3fca04faa09302e8ULL);
  arg3 = vcreate_f64 (0x3fecfec8c67415a0ULL);

  expected = 0.6437868393361155;
  actual = vget_lane_f64 (vmls_f64 (arg1, arg2, arg3), 0);

  if (__builtin_fabs (expected - actual) > EPS)
    abort ();

  return 0;
}
