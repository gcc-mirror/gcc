/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vuint32m8_t,
  uint16_t,
  __riscv_vle16_v_u16m4,
  __riscv_vwmulu_vx_u32m8,
  __riscv_vse32_v_u32m8,
  vwmulu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4)

/* { dg-final { scan-assembler-times {vwmulu\.vx\s+v0,v4,} 1 } } */
/* { dg-final { scan-assembler-times {vwmulu\.vx\s+v8,v12,} 1 } } */
