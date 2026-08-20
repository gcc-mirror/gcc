/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m1,
  vuint16m1_t,
  vuint32m2_t,
  uint16_t,
  __riscv_vle16_v_u16m1,
  __riscv_vwaddu_vx_u32m2,
  __riscv_vse32_v_u32m2,
  vwaddu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* { dg-final { scan-assembler-times {vwaddu\.vx\s+v0,v1,} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.vx\s+v2,v3,} 1 } } */
