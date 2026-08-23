/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m4,
  vint32m4_t,
  vint64m8_t,
  uint32_t,
  __riscv_vle32_v_i32m4,
  __riscv_vwmulsu_vx_i64m8,
  __riscv_vse64_v_i64m8,
  vwmulsu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4)

/* { dg-final { scan-assembler-times {vwmulsu\.vx\s+v0,v4,} 1 } } */
/* { dg-final { scan-assembler-times {vwmulsu\.vx\s+v8,v12,} 1 } } */
