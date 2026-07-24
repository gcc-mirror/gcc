/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e8m4,
  vuint8m4_t,
  vuint16m8_t,
  __riscv_vle8_v_u8m4,
  __riscv_vle16_v_u16m8,
  __riscv_vwaddu_wv_u16m8,
  __riscv_vse16_v_u16m8,
  vwaddu_wv,
  LOOP_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v0,v0,v12} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v8,v8,v28} 1 } } */
