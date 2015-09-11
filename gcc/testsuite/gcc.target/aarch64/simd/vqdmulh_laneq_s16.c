/* Test the vqdmulh_laneq_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int16x4_t
t_vqdmulh_laneq_s16 (int16x4_t a, int16x8_t b)
{
  return vqdmulh_laneq_s16 (a, b, 0);
}

/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[vV\]\[0-9\]+\.4\[hH\], ?\[vV\]\[0-9\]+\.4\[hH\], ?\[vV\]\[0-9\]+\.\[hH\]\\\[0\\\]\n" 1 } } */
