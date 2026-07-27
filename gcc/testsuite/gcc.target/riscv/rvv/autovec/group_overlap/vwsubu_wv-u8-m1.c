/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  __riscv_vle8_v_u8m1,
  __riscv_vle16_v_u16m2,
  __riscv_vwsubu_wv_u16m2,
  __riscv_vse16_v_u16m2,
  vwsubu_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vwsubu\.wv\s+v0,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.wv\s+v2,v2,v25} 1 } } */
