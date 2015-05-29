/* Test the vqdmull_high_laneq_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int32x4_t
t_vqdmull_high_laneq_s16 (int16x8_t a, int16x8_t b)
{
  return vqdmull_high_laneq_s16 (a, b, 0);
}

/* { dg-final { scan-assembler-times "sqdmull2\[ \t\]+\[vV\]\[0-9\]+\.4\[sS\], ?\[vV\]\[0-9\]+\.8\[hH\], ?\[vV\]\[0-9\]+\.\[hH\]\\\[0\\\]\n" 1 } } */
