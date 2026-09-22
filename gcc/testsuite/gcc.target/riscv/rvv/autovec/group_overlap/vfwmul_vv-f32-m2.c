/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e32m2,
  vfloat32m2_t,
  vfloat64m4_t,
  __riscv_vle32_v_f32m2,
  __riscv_vfwmul_vv_f64m4,
  __riscv_vse64_v_f64m4,
  vfwmul_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vfwmul\.vv\s+v8,v10,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmul\.vv\s+v28,v30,v24([^0-9]|$)} 1 } } */
