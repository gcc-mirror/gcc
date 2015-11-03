/* Test the vmulxd_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

void __attribute__ ((noinline))
test_case (float64_t v1, float64_t v2, float64_t e1)
{
  float64_t actual1 = vmulxd_f64 (v1, v2);
  if (actual1 != e1)
    abort ();
}

int
main (void)
{
  int i;
  float64_t v1 = 3.14159265359;
  float64_t v2 = 1.383894;
  float64_t v3 = -2.71828;
  float64_t v4 = -3.4891931;

  test_case (v1, v2, v1 * v2);
  test_case (0.0, __builtin_huge_val (), 2.0);
  test_case (0.0, -__builtin_huge_val (), -2.0);
  test_case (-0.0, __builtin_huge_val (), -2.0);
  test_case (-0.0, -__builtin_huge_val (), 2.0);

  return 0;
}

/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 1 } } */
