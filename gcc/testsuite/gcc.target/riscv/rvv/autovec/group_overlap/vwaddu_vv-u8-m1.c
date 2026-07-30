/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  __riscv_vle8_v_u8m1,
  __riscv_vwaddu_vv_u16m2,
  __riscv_vse16_v_u16m2,
  vwaddu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vwaddu\.vv\s+v2,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.vv\s+v10,v11,v3([^0-9]|$)} 1 } } */
