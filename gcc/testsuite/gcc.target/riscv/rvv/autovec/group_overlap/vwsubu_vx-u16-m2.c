/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m2,
  vuint16m2_t,
  vuint32m4_t,
  uint16_t,
  __riscv_vle16_v_u16m2,
  __riscv_vwsubu_vx_u32m4,
  __riscv_vse32_v_u32m4,
  vwsubu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8)

/* { dg-final { scan-assembler-times {vwsubu\.vx\s+v0,v2,} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.vx\s+v4,v6,} 1 } } */
