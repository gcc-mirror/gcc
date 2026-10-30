/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfbfwma -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u16m4_bf16m4 ( \
    __riscv_vreinterpret_v_u32m4_u16m4 (__riscv_vreinterpret_v_f32m4_u32m4 (v)))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e16m2,
  vbfloat16m2_t,
  vfloat32m4_t,
  vbfloat16m4_t,
  __bf16,
  __riscv_vle32_v_f32m4,
  RI_F,
  __riscv_vget_v_bf16m4_bf16m2,
  1,
  __riscv_vfwmaccbf16_vf_f32m4,
  __riscv_vse32_v_f32m4,
  vfwmaccbf16_vf,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwmaccbf16\.vf\s+v12,fa0,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmaccbf16\.vf\s+v8,fa0,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwmaccbf16\.vf\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmaccbf16\.vf\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmaccbf16\.vf\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
