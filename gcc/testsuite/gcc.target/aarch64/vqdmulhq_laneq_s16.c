/* Test the vqdmulhq_laneq_s16 AArch64 SIMD intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

int16x8_t
t_vqdmulhq_laneq_s16 (int16x8_t a, int16x8_t b)
{
  return vqdmulhq_laneq_s16 (a, b, 0);
}

/* { dg-final { scan-assembler-times "sqdmulh\[ \t\]+\[vV\]\[0-9\]+\.8\[hH\], ?\[vV\]\[0-9\]+\.8\[hH\], ?\[vV\]\[0-9\]+\.\[hH\]\\\[0\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
