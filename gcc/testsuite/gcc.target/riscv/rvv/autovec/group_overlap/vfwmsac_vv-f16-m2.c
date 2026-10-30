/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u16m4_f16m4 ( \
    __riscv_vreinterpret_v_u32m4_u16m4 (__riscv_vreinterpret_v_f32m4_u32m4 (v)))

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m2,
  vfloat16m2_t,
  vfloat32m4_t,
  vfloat16m4_t,
  __riscv_vle16_v_f16m2,
  __riscv_vle32_v_f32m4,
  RI_F,
  __riscv_vget_v_f16m4_f16m2,
  1,
  __riscv_vfwmsac_vv_f32m4,
  __riscv_vse32_v_f32m4,
  vfwmsac_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v12,v10,v16([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v8,v2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vv\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vv\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vv\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vv\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vv\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vv\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
