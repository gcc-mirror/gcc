/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e8m2,
  vint8m2_t,
  vint16m4_t,
  __riscv_vle8_v_i8m2,
  __riscv_vwsub_vv_i16m4,
  __riscv_vse16_v_i16m4,
  vwsub_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vwsub\.vv\s+v8,v10,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwsub\.vv\s+v28,v30,v24([^0-9]|$)} 1 } } */
