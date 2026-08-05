/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_2(
  __riscv_vsetvlmax_e32m1,
  vint32m1_t,
  vuint32m1_t,
  vint64m2_t,
  __riscv_vle32_v_i32m1,
  __riscv_vle32_v_u32m1,
  __riscv_vwmulsu_vv_i64m2,
  __riscv_vse64_v_i64m2,
  vwmulsu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_SU_X16)

/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v2,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v10,v11,v3([^0-9]|$)} 1 } } */
