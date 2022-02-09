/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

void f1(int64x2_t *res, int64_t *x, int c0, int c1) {
  res[0] = (int64x2_t) { c0 ? x[0] : 0, c1 ? x[2] : 0 };
}

/* { dg-final { scan-assembler-times {\tldr\tx[0-9]+} 2 } } */
/* { dg-final { scan-assembler {\tstp\tx[0-9]+, x[0-9]+} } } */
/* { dg-final { scan-assembler-not {\tldr\td} } } */
