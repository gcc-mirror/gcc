/* Test the vmulxs_f32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

void __attribute__ ((noinline))
test_case (float32_t v1, float32_t v2, float32_t e)
{
  float32_t actual = vmulxs_f32 (v1, v2);
  if (actual != e)
    abort ();
}

int
main (void)
{
  float32_t v1 = 3.14159265359;
  float32_t v2 = 1.383894;
  float32_t v3 = -2.71828;
  float32_t v4 = -3.4891931;

  test_case (v1, v2, v1 * v2);
  test_case (0.0, __builtin_huge_valf (), 2.0);
  test_case (0.0, -__builtin_huge_valf (), -2.0);
  test_case (-0.0, __builtin_huge_valf (), -2.0);
  test_case (-0.0, -__builtin_huge_valf (), 2.0);

  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+\n" 1 } } */
