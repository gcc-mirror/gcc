/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
#include "riscv_vector.h"

void f1 (void * in, void *out)
{
  vbool32_t mask = __riscv_vlm_v_b32 (in + 100, 4);
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
  vbool32_t v3 = __riscv_vmsbc_vvm_i32m1_b32 (v2, v2, mask, 4);
  __riscv_vsm_v_b32 (out, v3, 4);
}

void f2 (void * in, void *out)
{
  vbool32_t mask = *(vbool32_t*)in;
  asm volatile ("":::"memory");
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  vint32m1_t v2 = __riscv_vle32_v_i32m1_tumu (mask, v, in, 4);
  vbool32_t v3 = __riscv_vmsbc_vvm_i32m1_b32 (v2, v2, mask, 4);
  __riscv_vsm_v_b32 (out, v3, 4);
}

/* { dg-final { scan-assembler-times {vmsbc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-not {vmv} } } */
