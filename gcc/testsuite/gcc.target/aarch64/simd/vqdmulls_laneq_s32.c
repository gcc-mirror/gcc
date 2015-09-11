/* Test the vqdmulls_laneq_s32 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int64_t
t_vqdmulls_laneq_s32 (int32_t a, int32x4_t b)
{
  return vqdmulls_laneq_s32 (a, b, 0);
}

/* { dg-final { scan-assembler-times "sqdmull\[ \t\]+\[dD\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
