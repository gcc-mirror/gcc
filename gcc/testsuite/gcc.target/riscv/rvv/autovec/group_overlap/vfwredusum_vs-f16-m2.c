/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

static vfloat32m2_t
reinterpret_f16m2_f32m2 (vfloat16m2_t v)
{
  vuint16m2_t u = __riscv_vreinterpret_v_f16m2_u16m2 (v);
  vuint32m2_t wu = __riscv_vreinterpret_v_u16m2_u32m2 (u);

  return __riscv_vreinterpret_v_u32m2_f32m2 (wu);
}

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e16m2,
  vfloat16m2_t,
  vfloat32m1_t,
  vfloat32m2_t,
  __riscv_vle16_v_f16m2,
  reinterpret_f16m2_f32m2,
  __riscv_vget_v_f32m2_f32m1,
  1,
  __riscv_vfwredusum_vs_f16m2_f32m1,
  __riscv_vse32_v_f32m1,
  vfwredusum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v4,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v2,v2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwredusum\.vs\s+v[0-9]+,v2,v(2|3)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwredusum\.vs\s+v[0-9]+,v4,v(4|5)([^0-9]|$)} } } */
