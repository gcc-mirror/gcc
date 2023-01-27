/* Test the vqrdmulhs_lane_s32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"
#include <stdio.h>

extern void abort (void);

int32_t __attribute__((noipa))
test_vqrdmulhs (int32_t arg1, int32x2_t arg2)
{
  return vqrdmulhs_lane_s32 (arg1, arg2, 1);
}

int
main (void)
{
  int32_t actual;
  int32_t expected;

  actual = test_vqrdmulhs (-2099281921, vcreate_s32 (0x000080007fff0000ULL));
  expected = -32033;

  if (expected != actual)
    {
      fprintf (stderr, "Expected: %xd, got %xd\n", expected, actual);
      abort ();
    }

  return 0;
}

/* { dg-final { scan-assembler-times "sqrdmulh\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[sS\]\\\[1\\\]\n" 1 } } */
