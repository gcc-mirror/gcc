/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u16m4_f16m4 ( \
    __riscv_vreinterpret_v_u32m4_u16m4 (__riscv_vreinterpret_v_f32m4_u32m4 (v)))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e16m2,
  vfloat16m2_t,
  vfloat32m4_t,
  vfloat16m4_t,
  _Float16,
  __riscv_vle32_v_f32m4,
  RI_F,
  __riscv_vget_v_f16m4_f16m2,
  1,
  __riscv_vfwnmacc_vf_f32m4,
  __riscv_vse32_v_f32m4,
  vfwnmacc_vf,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwnmacc\.vf\s+v8,fa0,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwnmacc\.vf\s+v4,fa0,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vf\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vf\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
