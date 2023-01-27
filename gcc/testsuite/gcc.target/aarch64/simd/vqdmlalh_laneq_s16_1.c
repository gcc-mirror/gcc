/* Test the vqdmlalh_laneq_s16 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

int32_t __attribute__((noipa))
test_vqdmlalh (int32_t arg1, int16_t arg2, int16x8_t arg3)
{
  return vqdmlalh_laneq_s16 (arg1, arg2, arg3, 7);
}

int
main (void)
{
  int32_t actual;
  int32_t expected;

  actual = test_vqdmlalh (0x80000000, -24497,
			  vcombine_s16 (vcreate_s16 (0x008a80007fff7fffULL),
					vcreate_s16 (0xfffffa797fff8000ULL)));
  expected = -2147434654;

  if (expected != actual)
    abort ();

  return 0;
}


/* { dg-final { scan-assembler-times "sqdmlal\[ \t\]+\[sS\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[7\\\]\n" 1 } } */
