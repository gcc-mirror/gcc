/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

static vfloat32m8_t
reinterpret_f16m8_f32m8 (vfloat16m8_t v)
{
  vuint16m8_t u = __riscv_vreinterpret_v_f16m8_u16m8 (v);
  vuint32m8_t wu = __riscv_vreinterpret_v_u16m8_u32m8 (u);

  return __riscv_vreinterpret_v_u32m8_f32m8 (wu);
}

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e16m8,
  vfloat16m8_t,
  vfloat32m1_t,
  vfloat32m8_t,
  __riscv_vle16_v_f16m8,
  reinterpret_f16m8_f32m8,
  __riscv_vget_v_f32m8_f32m1,
  7,
  __riscv_vfwredosum_vs_f16m8_f32m1,
  __riscv_vse32_v_f32m1,
  vfwredosum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v16,v16,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v8,v8,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v8,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v16,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
