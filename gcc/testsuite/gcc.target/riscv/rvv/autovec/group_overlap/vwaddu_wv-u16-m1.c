/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e16m1,
  vuint16m1_t,
  vuint32m2_t,
  __riscv_vle16_v_u16m1,
  __riscv_vle32_v_u32m2,
  __riscv_vwaddu_wv_u32m2,
  __riscv_vse32_v_u32m2,
  vwaddu_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v0,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v2,v2,v25} 1 } } */
