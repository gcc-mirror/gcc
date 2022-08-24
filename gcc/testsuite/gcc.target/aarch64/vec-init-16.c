/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

void f1(int32x2_t *res, int32_t *x, int c0, int c1) {
  res[0] = (int32x2_t) { c0 ? x[0] : 0, c1 ? x[2] : 0 };
}

/* { dg-final { scan-assembler-times {\tldr\tw[0-9]+} 2 } } */
/* { dg-final { scan-assembler {\tstp\tw[0-9]+, w[0-9]+} } } */
/* { dg-final { scan-assembler-not {\tldr\ts} } } */
