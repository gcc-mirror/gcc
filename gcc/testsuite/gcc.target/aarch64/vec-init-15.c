/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

int32x2_t f1(int32_t *x, int c) {
  return c ? (int32x2_t) { x[0], x[2] } : (int32x2_t) { 0, 0 };
}

int32x2_t f2(int32_t *x, int i0, int i1, int c) {
  return c ? (int32x2_t) { x[i0], x[i1] } : (int32x2_t) { 0, 0 };
}

/* { dg-final { scan-assembler-times {\t(?:ldr\ts[0-9]+|ld1\t)} 4 } } */
/* { dg-final { scan-assembler-not {\tldr\tw} } } */
