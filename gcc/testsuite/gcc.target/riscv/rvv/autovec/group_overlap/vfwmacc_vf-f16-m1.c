/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u16m2_f16m2 ( \
    __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vreinterpret_v_f32m2_u32m2 (v)))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e16m1,
  vfloat16m1_t,
  vfloat32m2_t,
  vfloat16m2_t,
  _Float16,
  __riscv_vle32_v_f32m2,
  RI_F,
  __riscv_vget_v_f16m2_f16m1,
  1,
  __riscv_vfwmacc_vf_f32m2,
  __riscv_vse32_v_f32m2,
  vfwmacc_vf,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwmacc\.vf\s+v6,fa0,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmacc\.vf\s+v4,fa0,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwmacc\.vf\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmacc\.vf\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmacc\.vf\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
