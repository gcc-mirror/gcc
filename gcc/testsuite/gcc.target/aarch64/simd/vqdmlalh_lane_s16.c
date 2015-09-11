/* Test the vqdmlalh_lane_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32_t
t_vqdmlalh_lane_s16 (int32_t a, int16_t b, int16x4_t c)
{
  return vqdmlalh_lane_s16 (a, b, c, 0);
}

/* { dg-final { scan-assembler-times "sqdmlal\[ \t\]+\[sS\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[0\\\]\n" 1 } } */
