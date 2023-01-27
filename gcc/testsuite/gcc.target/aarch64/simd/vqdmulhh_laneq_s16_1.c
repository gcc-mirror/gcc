/* Test the vqdmulhh_laneq_s16 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

int16_t __attribute__((noipa))
test_vqdmulhh (int16_t arg1, int16x8_t arg2)
{
  return vqdmulhh_laneq_s16 (arg1, arg2, 7);
}

int
main (void)
{
  int16_t actual;
  int16_t expected;

  actual = test_vqdmulhh (268,
			  vcombine_s16 (vcreate_s16 (0xffffffff00000000ULL),
					vcreate_s16 (0x0000800018410000ULL)));
  expected = 0;

  if (expected != actual)
    abort ();

  return 0;
}


/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[hH\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[7\\\]\n" 1 } } */
