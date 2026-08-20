/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  uint8_t,
  __riscv_vle8_v_u8m1,
  __riscv_vwsubu_vx_u16m2,
  __riscv_vse16_v_u16m2,
  vwsubu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* { dg-final { scan-assembler-times {vwsubu\.vx\s+v0,v1,} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.vx\s+v2,v3,} 1 } } */
