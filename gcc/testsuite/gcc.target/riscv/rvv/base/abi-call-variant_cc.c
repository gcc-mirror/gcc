/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1" } */

#include "riscv_vector.h"

void
f_undef1 (vint8m1_t a);
void
f_undef2 (vint8m1x8_t a);
void
f_undef3 (vbool1_t a);
vint8m1_t
f_undef4 ();

void
bar_real (vint8m1_t a, vint8m1x8_t b, vbool1_t c)
{
  f_undef1 (a);
  f_undef2 (b);
  f_undef3 (c);
}

__attribute__ ((alias ("bar_real"))) void
bar_alias (vint8m1_t a, vint8m1x8_t b, vbool1_t c);

void
f_1 (vint8m1_t *a, vint8m1x8_t *b, vbool1_t *c)
{
  bar_alias (*a, *b, *c);
  *a = f_undef4 ();
}

/* { dg-final { scan-assembler-times {\.variant_cc\tbar_real} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tbar_alias} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tf_1} 0 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tf_undef1} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tf_undef2} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tf_undef3} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tf_undef4} 1 } } */
