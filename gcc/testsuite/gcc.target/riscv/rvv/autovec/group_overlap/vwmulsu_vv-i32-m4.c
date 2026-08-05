/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_2(
  __riscv_vsetvlmax_e32m4,
  vint32m4_t,
  vuint32m4_t,
  vint64m8_t,
  __riscv_vle32_v_i32m4,
  __riscv_vle32_v_u32m4,
  __riscv_vwmulsu_vv_i64m8,
  __riscv_vse64_v_i64m8,
  vwmulsu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_SU_X4)

/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v24,v28,v16([^0-9]|$)} 1 } } */
