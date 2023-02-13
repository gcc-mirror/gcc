/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */
#include "riscv_vector.h"

void f (void * in, void *out, int64_t x, int n)
{
  vuint64m1_t v = __riscv_vle64_v_u64m1 (in + 1, 4);
  vuint64m1_t v2 = __riscv_vle64_v_u64m1_tu (v, in + 2, 4);
  vbool64_t mask = __riscv_vlm_v_b64 (in,8);
  vbool64_t mask2 = __riscv_vlm_v_b64 (in + 100,8);
  vbool64_t v3 = __riscv_vmsgeu_vx_u64m1_b64_mu (mask,mask2,v2, 0, 4);
  __riscv_vsm_v_b64 (out + 2, v3, 4);
}

/* { dg-final { scan-assembler-times {vmor\.mm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
