/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vfloat64m4_t
reinterpret_f32m4_f64m4 (vfloat32m4_t v)
{
  vuint32m4_t u = __riscv_vreinterpret_v_f32m4_u32m4 (v);
  vuint64m4_t wu = __riscv_vreinterpret_v_u32m4_u64m4 (u);

  return __riscv_vreinterpret_v_u64m4_f64m4 (wu);
}

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e32m4,
  vfloat32m4_t,
  vfloat64m1_t,
  vfloat64m4_t,
  __riscv_vle32_v_f32m4,
  reinterpret_f32m4_f64m4,
  __riscv_vget_v_f64m4_f64m1,
  3,
  __riscv_vfwredusum_vs_f32m4_f64m1,
  __riscv_vse64_v_f64m1,
  vfwredusum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v8,v8,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v4,v4,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwredusum\.vs\s+v[0-9]+,v4,v(4|5|6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwredusum\.vs\s+v[0-9]+,v8,v(8|9|10|11)([^0-9]|$)} } } */
