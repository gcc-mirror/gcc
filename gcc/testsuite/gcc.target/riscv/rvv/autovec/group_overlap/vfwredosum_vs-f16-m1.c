/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

static vfloat32m1_t
reinterpret_f16m1_f32m1 (vfloat16m1_t v)
{
  vuint16m1_t u = __riscv_vreinterpret_v_f16m1_u16m1 (v);
  vuint32m1_t wu = __riscv_vreinterpret_v_u16m1_u32m1 (u);

  return __riscv_vreinterpret_v_u32m1_f32m1 (wu);
}

DEF_GROUP_OVERLAP_REDUCE_1(
  __riscv_vsetvlmax_e16m1,
  vfloat16m1_t,
  vfloat32m1_t,
  __riscv_vle16_v_f16m1,
  reinterpret_f16m1_f32m1,
  __riscv_vfwredosum_vs_f16m1_f32m1,
  __riscv_vse32_v_f32m1,
  vfwredosum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_LMUL1_X2)

/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v0,v2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v5,v1,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v1,v1([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v2,v2([^0-9]|$)} } } */
