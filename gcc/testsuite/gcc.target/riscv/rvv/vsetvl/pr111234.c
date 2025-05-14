/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
f (vint32m1_t *in, vint64m2_t *out, vbool32_t *m, int b)
{
  vint32m1_t va = *in;
  vbool32_t mask = *m;
  vint64m2_t vb
    = __riscv_vwadd_vx_i64m2_m (mask, va, 1, __riscv_vsetvlmax_e64m2 ());
  vint64m2_t vc = __riscv_vadd_vx_i64m2 (vb, 1, __riscv_vsetvlmax_e64m2 ());

  if (b != 0)
    vc = __riscv_vadd_vx_i64m2_mu (mask, vc, vc, 1, __riscv_vsetvlmax_e64m2 ());

  *out = vc;
}
