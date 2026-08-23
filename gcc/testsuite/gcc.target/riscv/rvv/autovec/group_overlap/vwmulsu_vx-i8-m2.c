/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e8m2,
  vint8m2_t,
  vint16m4_t,
  uint8_t,
  __riscv_vle8_v_i8m2,
  __riscv_vwmulsu_vx_i16m4,
  __riscv_vse16_v_i16m4,
  vwmulsu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8)

/* { dg-final { scan-assembler-times {vwmulsu\.vx\s+v0,v2,} 1 } } */
/* { dg-final { scan-assembler-times {vwmulsu\.vx\s+v4,v6,} 1 } } */
