/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

int
foo (void *in, void *out)
{
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  v = __riscv_vadd_vv_i32m1 (v, v, 4);
  vbool32_t mask = __riscv_vreinterpret_v_i32m1_b32 (v);
  return __riscv_vfirst_m_b32 (mask, 4);
}

int
foo2 (void *in, void *out)
{
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  v = __riscv_vadd_vv_i32m1 (v, v, 4);
  vbool32_t mask = __riscv_vreinterpret_v_i32m1_b32 (v);
  mask = __riscv_vmsbf_m_b32 (mask, 4);
  return __riscv_vfirst_m_b32 (mask, 4);
}

int
foo3 (void *in, void *out)
{
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  v = __riscv_vadd_vv_i32m1 (v, v, 4);
  vbool32_t mask = __riscv_vreinterpret_v_i32m1_b32 (v);
  mask = __riscv_vmsif_m_b32 (mask, 4);
  return __riscv_vfirst_m_b32 (mask, 4);
}

int
foo4 (void *in, void *out)
{
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  v = __riscv_vadd_vv_i32m1 (v, v, 4);
  vbool32_t mask = __riscv_vreinterpret_v_i32m1_b32 (v);
  mask = __riscv_vmsof_m_b32 (mask, 4);
  return __riscv_vfirst_m_b32 (mask, 4);
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e32,\s*m1,\s*t[au],\s*m[au]} 4 } } */
/* { dg-final { scan-assembler-times {vsetivli} 4 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
