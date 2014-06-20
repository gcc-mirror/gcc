/* Test the vqdmlsl_lane_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32x4_t
t_vqdmlsl_lane_s16 (int32x4_t a, int16x4_t b, int16x4_t c)
{
  return vqdmlsl_lane_s16 (a, b, c, 0);
}

/* { dg-final { scan-assembler-times "sqdmlsl\[ \t\]+\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.4\[hH\], ?\[vV\]\[0-9\]+\.\[hH\]\\\[0\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
