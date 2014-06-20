/* Test the vqrdmulh_laneq_s32 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32x2_t
t_vqrdmulh_laneq_s32 (int32x2_t a, int32x4_t b)
{
  return vqrdmulh_laneq_s32 (a, b, 0);
}

/* { dg-final { scan-assembler-times "sqrdmulh\[ \t\]+\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.2\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
