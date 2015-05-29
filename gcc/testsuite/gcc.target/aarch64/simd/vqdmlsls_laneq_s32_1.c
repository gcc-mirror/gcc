/* Test the vqdmlsls_laneq_s32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

extern void abort (void);

int
main (void)
{
  int64_t arg1;
  int32_t arg2;
  int32x4_t arg3;
  int64_t actual;
  int64_t expected;

  arg1 = 140733193453567LL;
  arg2 = 25544;
  arg3 = vcombine_s32 (vcreate_s32 (0x417b8000ffff8397LL),
                       vcreate_s32 (0x7fffffff58488000LL));


  actual = vqdmlsls_laneq_s32 (arg1, arg2, arg3, 3);
  expected = 31022548895631LL;

  if (expected != actual)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "sqdmlsl\[ \t\]+\[dD\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[sS\]\\\[3\\\]\n" 1 } } */
