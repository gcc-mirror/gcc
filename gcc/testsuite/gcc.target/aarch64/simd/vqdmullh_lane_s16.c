/* Test the vqdmullh_lane_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32_t
t_vqdmullh_lane_s16 (int16_t a, int16x4_t b)
{
  return vqdmullh_lane_s16 (a, b, 0);
}

/* { dg-final { scan-assembler-times "sqdmull\[ \t\]+\[sS\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[0\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
