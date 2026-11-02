/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

static vfloat32m4_t
reinterpret_f16m4_f32m4 (vfloat16m4_t v)
{
  vuint16m4_t u = __riscv_vreinterpret_v_f16m4_u16m4 (v);
  vuint32m4_t wu = __riscv_vreinterpret_v_u16m4_u32m4 (u);

  return __riscv_vreinterpret_v_u32m4_f32m4 (wu);
}

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e16m4,
  vfloat16m4_t,
  vfloat32m1_t,
  vfloat32m4_t,
  __riscv_vle16_v_f16m4,
  reinterpret_f16m4_f32m4,
  __riscv_vget_v_f32m4_f32m1,
  3,
  __riscv_vfwredosum_vs_f16m4_f32m1,
  __riscv_vse32_v_f32m1,
  vfwredosum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v8,v8,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v4,v4,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v4,v(4|5|6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v8,v(8|9|10|11)([^0-9]|$)} } } */
