/* Test the vqdmulhs_lane_s32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"
#include <stdio.h>

extern void abort (void);

int
main (void)
{
  int32_t arg1;
  int32x2_t arg2;
  int32_t result;
  int32_t actual;
  int32_t expected;

  arg1 = 57336;
  arg2 = vcreate_s32 (0x55897fff7fff0000ULL);
  actual = vqdmulhs_lane_s32 (arg1, arg2, 0);
  expected = 57334;

  if (expected != actual)
    {
      fprintf (stderr, "Expected: %xd, got %xd\n", expected, actual);
      abort ();
    }

  return 0;
}
/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
