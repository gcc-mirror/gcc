/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc_xtheadvector -mabi=lp64d -O3" } */

#include "riscv_vector.h"

struct a
{
  int b[];
} c (vuint32m4_t), d;

char e;
char *f;

void g ()
{
  int h;
  vuint32m4_t i;
  vuint8m1_t j = __riscv_vlse8_v_u8m1 (&e, d.b[3], h);
  vuint16m2_t k = __riscv_vwaddu_vx_u16m2 (j, 0, h);
  i = __riscv_vwmaccu_vx_u32m4 (i, f[0], k, h);
  c (i);
}

/* { dg-final { scan-assembler-not {th\.vzext\.vf2} } } */
