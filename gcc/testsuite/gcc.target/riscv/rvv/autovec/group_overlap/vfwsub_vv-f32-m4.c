/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e32m4,
  vfloat32m4_t,
  vfloat64m8_t,
  __riscv_vle32_v_f32m4,
  __riscv_vfwsub_vv_f64m8,
  __riscv_vse64_v_f64m8,
  vfwsub_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vfwsub\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwsub\.vv\s+v24,v28,v16([^0-9]|$)} 1 } } */
