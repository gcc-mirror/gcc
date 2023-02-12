/* Test the vqdmlals_laneq_s32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

int64_t __attribute__((noipa))
test_vqdmlals (int64_t arg1, int32_t arg2, int32x4_t arg3)
{
  return vqdmlals_laneq_s32 (arg1, arg2, arg3, 3);
}

int
main (void)
{
  int64_t actual;
  int64_t expected;

  actual = test_vqdmlals (-9223182289494545592LL, 32768,
			  vcombine_s32 (vcreate_s32 (0xffff7fff8000ffffULL),
					vcreate_s32 (0x80000000ffff0000ULL)));
  expected = -9223323026982900920LL;

  if (expected != actual)
    abort ();

  return 0;
}


/* { dg-final { scan-assembler-times "sqdmlal\[ \t\]+\[dD\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[sS\]\\\[3\\\]\n" 1 } } */
