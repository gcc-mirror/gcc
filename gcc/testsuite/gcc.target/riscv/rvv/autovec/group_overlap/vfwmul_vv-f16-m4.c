/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e16m4,
  vfloat16m4_t,
  vfloat32m8_t,
  __riscv_vle16_v_f16m4,
  __riscv_vfwmul_vv_f32m8,
  __riscv_vse32_v_f32m8,
  vfwmul_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vfwmul\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmul\.vv\s+v24,v28,v16([^0-9]|$)} 1 } } */
