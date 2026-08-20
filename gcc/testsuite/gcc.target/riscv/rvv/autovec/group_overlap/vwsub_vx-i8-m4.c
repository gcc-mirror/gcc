/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e8m4,
  vint8m4_t,
  vint16m8_t,
  int8_t,
  __riscv_vle8_v_i8m4,
  __riscv_vwsub_vx_i16m8,
  __riscv_vse16_v_i16m8,
  vwsub_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4)

/* { dg-final { scan-assembler-times {vwsub\.vx\s+v0,v4,} 1 } } */
/* { dg-final { scan-assembler-times {vwsub\.vx\s+v8,v12,} 1 } } */
