/* Test the vqdmlslh_lane_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32_t
t_vqdmlslh_lane_s16 (int32_t a, int16_t b, int16x4_t c)
{
  return vqdmlslh_lane_s16 (a, b, c, 0);
}

/* { dg-final { scan-assembler-times "sqdmlsl\[ \t\]+\[sS\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[hH\]\[0-9\]\n" 1 } } */
