/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

#include "riscv_vector.h"
void
f (float x, float y, void *out)
{
  float f[4] = { x, x, x, y };
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (f, 4);
  __riscv_vse32_v_f32m1 (out, v, 4);
}

/* { dg-final { scan-assembler-not {vmv} } } */
