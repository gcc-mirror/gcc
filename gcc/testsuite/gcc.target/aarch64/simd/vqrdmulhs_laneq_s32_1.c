/* Test the vqrdmulhs_laneq_s32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);

int32_t __attribute__((noipa))
test_vqrdmulhs (int32_t arg1, int32x4_t arg2)
{
  return vqrdmulhs_laneq_s32 (arg1, arg2, 3);
}

int
main (void)
{
  int32_t actual;
  int32_t expected;

  actual = test_vqrdmulhs (32768,
			   vcombine_s32 (vcreate_s32 (0x8000ffffffffcd5bULL),
					 vcreate_s32 (0x7fffffffffffffffULL)));
  expected = 32768;

  if (expected != actual)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "sqrdmulh\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[sS\]\\\[3\\\]\n" 1 } } */
