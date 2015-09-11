/* Test the vqdmlsl_high_lane_s32 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int64x2_t
t_vqdmlsl_high_lane_s32 (int64x2_t a, int32x4_t b, int32x2_t c)
{
  return vqdmlsl_high_lane_s32 (a, b, c, 0);
}

/* { dg-final { scan-assembler-times "sqdmlsl2\[ \t\]+\[vV\]\[0-9\]+\.2\[dD\], ?\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.\[sS\]\\\[0\\\]\n" 1 } } */
