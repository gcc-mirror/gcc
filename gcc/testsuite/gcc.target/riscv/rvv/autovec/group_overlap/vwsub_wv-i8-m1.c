/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e8m1,
  vint8m1_t,
  vint16m2_t,
  __riscv_vle8_v_i8m1,
  __riscv_vle16_v_i16m2,
  __riscv_vwsub_wv_i16m2,
  __riscv_vse16_v_i16m2,
  vwsub_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vwsub\.wv\s+v0,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwsub\.wv\s+v2,v2,v25} 1 } } */
