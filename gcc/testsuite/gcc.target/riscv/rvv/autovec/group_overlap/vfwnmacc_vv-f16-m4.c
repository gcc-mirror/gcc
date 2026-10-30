/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u16m8_f16m8 ( \
    __riscv_vreinterpret_v_u32m8_u16m8 (__riscv_vreinterpret_v_f32m8_u32m8 (v)))

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m4,
  vfloat16m4_t,
  vfloat32m8_t,
  vfloat16m8_t,
  __riscv_vle16_v_f16m4,
  __riscv_vle32_v_f32m8,
  RI_F,
  __riscv_vget_v_f16m8_f16m4,
  1,
  __riscv_vfwnmacc_vv_f32m8,
  __riscv_vse32_v_f32m8,
  vfwnmacc_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwnmacc\.vv\s+v16,v0,v28([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwnmacc\.vv\s+v8,v24,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v16,v(16|17|18|19|20|21|22|23),} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v8,v(8|9|10|11|12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
