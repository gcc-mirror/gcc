/* Test the vqdmulhh_lane_s16 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"
#include <stdio.h>

extern void abort (void);

int
main (void)
{
  int16_t arg1;
  int16x4_t arg2;
  int16_t result;
  int16_t actual;
  int16_t expected;

  arg1 = -32768;
  arg2 = vcreate_s16 (0x0000ffff2489e398ULL);
  actual = vqdmulhh_lane_s16 (arg1, arg2, 2);
  expected = 1;

  if (expected != actual)
    {
      fprintf (stderr, "Expected: %xd, got %xd\n", expected, actual);
      abort ();
    }

  return 0;
}


/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[hH\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[2\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
