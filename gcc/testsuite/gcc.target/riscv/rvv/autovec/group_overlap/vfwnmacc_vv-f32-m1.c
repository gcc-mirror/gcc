/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m2_f32m2 ( \
    __riscv_vreinterpret_v_u64m2_u32m2 (__riscv_vreinterpret_v_f64m2_u64m2 (v)))

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e32m1,
  vfloat32m1_t,
  vfloat64m2_t,
  vfloat32m2_t,
  __riscv_vle32_v_f32m1,
  __riscv_vle64_v_f64m2,
  RI_F,
  __riscv_vget_v_f32m2_f32m1,
  1,
  __riscv_vfwnmacc_vv_f64m2,
  __riscv_vse64_v_f64m2,
  vfwnmacc_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwnmacc\.vv\s+v4,v8,v7([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwnmacc\.vv\s+v2,v6,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v4,v(4|5),} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v2,v(2|3),} } } */
/* { dg-final { scan-assembler-not {vfwnmacc\.vv\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
