/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vfloat64m8_t
reinterpret_f32m8_f64m8 (vfloat32m8_t v)
{
  vuint32m8_t u = __riscv_vreinterpret_v_f32m8_u32m8 (v);
  vuint64m8_t wu = __riscv_vreinterpret_v_u32m8_u64m8 (u);

  return __riscv_vreinterpret_v_u64m8_f64m8 (wu);
}

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e32m8,
  vfloat32m8_t,
  vfloat64m1_t,
  vfloat64m8_t,
  __riscv_vle32_v_f32m8,
  reinterpret_f32m8_f64m8,
  __riscv_vget_v_f64m8_f64m1,
  7,
  __riscv_vfwredusum_vs_f32m8_f64m1,
  __riscv_vse64_v_f64m1,
  vfwredusum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v16,v16,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v8,v8,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwredusum\.vs\s+v[0-9]+,v8,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwredusum\.vs\s+v[0-9]+,v16,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
