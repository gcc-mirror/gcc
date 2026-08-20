/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e8m1,
  vint8m1_t,
  vint16m2_t,
  int8_t,
  __riscv_vle8_v_i8m1,
  __riscv_vwsub_vx_i16m2,
  __riscv_vse16_v_i16m2,
  vwsub_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* { dg-final { scan-assembler-times {vwsub\.vx\s+v0,v1,} 1 } } */
/* { dg-final { scan-assembler-times {vwsub\.vx\s+v2,v3,} 1 } } */
