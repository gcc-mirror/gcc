/* Test the vqdmulhq_laneq_s32 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32x4_t
t_vqdmulhq_laneq_s32 (int32x4_t a, int32x4_t b)
{
  return vqdmulhq_laneq_s32 (a, b, 0);
}

/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
