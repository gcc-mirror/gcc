/* Test the vmla_f64 AArch64 SIMD intrinsic.  */

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

  arg1 = vcreate_f64 (0x3fc4de626b6bbe9cULL);
  arg2 = vcreate_f64 (0x3fb7e454dbe84408ULL);
  arg3 = vcreate_f64 (0x3fdd359b94201a3aULL);

  expected = 0.20563116414665633;
  actual = vget_lane_f64 (vmla_f64 (arg1, arg2, arg3), 0);

  if (__builtin_fabs (expected - actual) > EPS)
    abort ();

  return 0;
}
