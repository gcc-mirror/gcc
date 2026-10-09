/* PR middle-end/121105 */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vint32m1x4_t
ret4 (void)
{
  vint32m1x4_t x = {};
  return x;
}

void
mem2 (vint8m1x2_t *p)
{
  *p = (vint8m1x2_t) {};
}

/* { dg-final { scan-assembler-times {(?:vmv\.v\.i|vmv1r\.v)\s+v(?:8|9|10|11),} 4 } } */
/* { dg-final { scan-assembler-times {vs1r\.v|vse8\.v} 2 } } */
