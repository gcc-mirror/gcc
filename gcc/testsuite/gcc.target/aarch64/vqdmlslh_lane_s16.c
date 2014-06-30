/* Test the vqdmlslh_lane_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32x1_t
t_vqdmlslh_lane_s16 (int32x1_t a, int16x1_t b, int16x4_t c)
{
  return vqdmlslh_lane_s16 (a, b, c, 0);
}

/* { dg-final { scan-assembler-times "sqdmlsl\[ \t\]+\[sS\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[0\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
