/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e16m1,
  vfloat16m1_t,
  vfloat32m2_t,
  __riscv_vle16_v_f16m1,
  __riscv_vfwmul_vv_f32m2,
  __riscv_vse32_v_f32m2,
  vfwmul_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vfwmul\.vv\s+v2,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmul\.vv\s+v10,v11,v3([^0-9]|$)} 1 } } */
