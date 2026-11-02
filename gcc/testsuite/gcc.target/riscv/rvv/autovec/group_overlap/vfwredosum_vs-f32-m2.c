/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vfloat64m2_t
reinterpret_f32m2_f64m2 (vfloat32m2_t v)
{
  vuint32m2_t u = __riscv_vreinterpret_v_f32m2_u32m2 (v);
  vuint64m2_t wu = __riscv_vreinterpret_v_u32m2_u64m2 (u);

  return __riscv_vreinterpret_v_u64m2_f64m2 (wu);
}

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e32m2,
  vfloat32m2_t,
  vfloat64m1_t,
  vfloat64m2_t,
  __riscv_vle32_v_f32m2,
  reinterpret_f32m2_f64m2,
  __riscv_vget_v_f64m2_f64m1,
  1,
  __riscv_vfwredosum_vs_f32m2_f64m1,
  __riscv_vse64_v_f64m1,
  vfwredosum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v4,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v2,v2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v2,v(2|3)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,v4,v(4|5)([^0-9]|$)} } } */
