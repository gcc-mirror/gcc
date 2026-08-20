/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e8m4,
  vuint8m4_t,
  vuint16m8_t,
  uint8_t,
  __riscv_vle8_v_u8m4,
  __riscv_vwsubu_vx_u16m8,
  __riscv_vse16_v_u16m8,
  vwsubu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4)

/* { dg-final { scan-assembler-times {vwsubu\.vx\s+v0,v4,} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.vx\s+v8,v12,} 1 } } */
