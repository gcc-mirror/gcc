/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc_xtheadvector -mabi=lp64d -O3" } */

#include "riscv_vector.h"

struct a
{
  int b[];
} c (vint32m4_t), d;

char e;
char *f;

void g ()
{
  int h;
  vint32m4_t i;
  vint8m1_t j = __riscv_vlse8_v_i8m1 (&e, d.b[3], h);
  vint16m2_t k = __riscv_vwadd_vx_i16m2 (j, 0, h);
  i = __riscv_vwmacc_vx_i32m4 (i, f[0], k, h);
  c (i);
}

/* { dg-final { scan-assembler-not {th\.vsext\.vf2} } } */
