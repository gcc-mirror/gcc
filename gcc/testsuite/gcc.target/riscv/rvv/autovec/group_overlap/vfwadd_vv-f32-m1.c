/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e32m1,
  vfloat32m1_t,
  vfloat64m2_t,
  __riscv_vle32_v_f32m1,
  __riscv_vfwadd_vv_f64m2,
  __riscv_vse64_v_f64m2,
  vfwadd_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vfwadd\.vv\s+v2,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd\.vv\s+v10,v11,v3([^0-9]|$)} 1 } } */
