/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e8m2,
  vuint8m2_t,
  vuint16m4_t,
  __riscv_vle8_v_u8m2,
  __riscv_vle16_v_u16m4,
  __riscv_vwaddu_wv_u16m4,
  __riscv_vse16_v_u16m4,
  vwaddu_wv,
  LOOP_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v0,v8,v2} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v16,v16,v0} 1 } } */
