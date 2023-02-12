/* Test the vqrdmulhh_lane_s16 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"
#include <stdio.h>

extern void abort (void);

int16_t __attribute__((noipa))
test_vqrdmulhh (int16_t arg1, int16x4_t arg2)
{
  return vqrdmulhh_lane_s16 (arg1, arg2, 3);
}

int
main (void)
{
  int16_t actual;
  int16_t expected;

  actual = test_vqrdmulhh (-32768, vcreate_s16 (0xd78e000005d78000ULL));
  expected = 10354;

  if (expected != actual)
    {
      fprintf (stderr, "Expected: %xd, got %xd\n", expected, actual);
      abort ();
    }

  return 0;
}

/* { dg-final { scan-assembler-times "sqrdmulh\[ \t\]+\[hH\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[3\\\]\n" 1 } } */
