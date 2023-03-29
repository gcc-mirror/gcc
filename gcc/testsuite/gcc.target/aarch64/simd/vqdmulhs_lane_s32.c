/* Test the vqdmulhs_lane_s32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"
#include <stdio.h>

extern void abort (void);

int32_t __attribute__((noipa))
test_vqdmulhs_0 (int32_t arg1, int32x2_t arg2)
{
  return vqdmulhs_lane_s32 (arg1, arg2, 0);
}

int32_t __attribute__((noipa))
test_vqdmulhs_1 (int32_t arg1, int32x2_t arg2)
{
  return vqdmulhs_lane_s32 (arg1, arg2, 1);
}

int
main (void)
{
  int32_t actual;
  int32_t expected;

  actual = test_vqdmulhs_0 (57336, vcreate_s32 (0x55897fff7fff0000ULL));
  expected = 57334;

  if (expected != actual)
    {
      fprintf (stderr, "Expected: %xd, got %xd\n", expected, actual);
      abort ();
    }

  actual = test_vqdmulhs_1 (57336, vcreate_s32 (0x55897fff7fff0000ULL));
  expected = 38315;

  if (expected != actual)
    {
      fprintf (stderr, "Expected: %xd, got %xd\n", expected, actual);
      abort ();
    }

  return 0;
}
/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?(?:\[sS\]\[0-9\]+|\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\])\n" 1 } } */
/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[sS\]\\\[1\\\]\n" 1 } } */
