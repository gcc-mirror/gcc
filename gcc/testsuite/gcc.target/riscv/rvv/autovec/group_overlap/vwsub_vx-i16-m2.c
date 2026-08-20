/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m2,
  vint16m2_t,
  vint32m4_t,
  int16_t,
  __riscv_vle16_v_i16m2,
  __riscv_vwsub_vx_i32m4,
  __riscv_vse32_v_i32m4,
  vwsub_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8)

/* { dg-final { scan-assembler-times {vwsub\.vx\s+v0,v2,} 1 } } */
/* { dg-final { scan-assembler-times {vwsub\.vx\s+v4,v6,} 1 } } */
