/* { dg-do compile } */
/* { dg-options "-march=armv8.2-a+sve -O2" }
/* PR tree-optimization/97405 */
#include "arm_sve.h"

void
a (svuint8x3_t b, unsigned char *p, int c) {
  if (c)
    svst1_u8(svptrue_pat_b8(SV_VL16), p, svget3_u8(b, 1));
  else
    svst1_u8(svwhilelt_b8(6, 6), p, svget3_u8(b, 1));
}

