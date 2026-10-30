/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m4_f32m4 ( \
    __riscv_vreinterpret_v_u64m4_u32m4 (__riscv_vreinterpret_v_f64m4_u64m4 (v)))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e32m2,
  vfloat32m2_t,
  vfloat64m4_t,
  vfloat32m4_t,
  float,
  __riscv_vle64_v_f64m4,
  RI_F,
  __riscv_vget_v_f32m4_f32m2,
  1,
  __riscv_vfwnmsac_vf_f64m4,
  __riscv_vse64_v_f64m4,
  vfwnmsac_vf,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwnmsac\.vf\s+v8,fa0,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwnmsac\.vf\s+v4,fa0,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwnmsac\.vf\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwnmsac\.vf\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
