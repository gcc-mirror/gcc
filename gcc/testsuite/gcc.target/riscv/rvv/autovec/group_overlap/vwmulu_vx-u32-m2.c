/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vuint64m4_t,
  uint32_t,
  __riscv_vle32_v_u32m2,
  __riscv_vwmulu_vx_u64m4,
  __riscv_vse64_v_u64m4,
  vwmulu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8)

/* { dg-final { scan-assembler-times {vwmulu\.vx\s+v0,v2,} 1 } } */
/* { dg-final { scan-assembler-times {vwmulu\.vx\s+v4,v6,} 1 } } */
