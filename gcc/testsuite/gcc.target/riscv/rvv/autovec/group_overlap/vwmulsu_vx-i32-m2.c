/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m2,
  vint32m2_t,
  vint64m4_t,
  uint32_t,
  __riscv_vle32_v_i32m2,
  __riscv_vwmulsu_vx_i64m4,
  __riscv_vse64_v_i64m4,
  vwmulsu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8)

/* { dg-final { scan-assembler-times {vwmulsu\.vx\s+v0,v2,} 1 } } */
/* { dg-final { scan-assembler-times {vwmulsu\.vx\s+v4,v6,} 1 } } */
