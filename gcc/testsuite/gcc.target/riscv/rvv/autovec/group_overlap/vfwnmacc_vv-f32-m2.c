/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m4_f32m4 ( \
    __riscv_vreinterpret_v_u64m4_u32m4 (__riscv_vreinterpret_v_f64m4_u64m4 (v)))

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e32m2,
  vfloat32m2_t,
  vfloat64m4_t,
  vfloat32m4_t,
  __riscv_vle32_v_f32m2,
  __riscv_vle64_v_f64m4,
  RI_F,
  __riscv_vget_v_f32m4_f32m2,
  1,
  __riscv_vfwnmacc_vv_f64m4,
  __riscv_vse64_v_f64m4,
  vfwnmacc_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwnmacc\.vv\s+v8,v16,v14([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwnmacc\.vv\s+v4,v12,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
