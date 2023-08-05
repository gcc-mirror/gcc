/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */
#include "riscv_vector.h"

void f (void * in, void *out, uint64_t x, int n)
{
  for (int i = 0; i < n; i++) {
    vuint64m1_t v = __riscv_vle64_v_u64m1 (in + i + 1, 4);
    vuint64m1_t v2 = __riscv_vle64_v_u64m1_tu (v, in + i + 2, 4);
    vuint64m1_t v3 = __riscv_vssubu_vx_u64m1 (v2, x, 4);
    vuint64m1_t v4 = __riscv_vssubu_vx_u64m1_tu (v3, v2, x, 4);
    __riscv_vse64_v_u64m1 (out + i + 2, v4, 4);
  }
}

/* { dg-final { scan-assembler-times {vlse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*zero\s+\.L[0-9]+\:\s+} 1 } } */
/* { dg-final { scan-assembler-times {vssubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-not {vmv} } } */
