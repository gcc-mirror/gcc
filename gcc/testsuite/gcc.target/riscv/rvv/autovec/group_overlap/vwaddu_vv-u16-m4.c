/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vuint32m8_t,
  __riscv_vle16_v_u16m4,
  __riscv_vwaddu_vv_u32m8,
  __riscv_vse32_v_u32m8,
  vwaddu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vwaddu\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.vv\s+v24,v28,v16([^0-9]|$)} 1 } } */
