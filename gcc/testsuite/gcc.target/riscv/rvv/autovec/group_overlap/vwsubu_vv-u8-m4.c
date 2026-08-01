/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e8m4,
  vuint8m4_t,
  vuint16m8_t,
  __riscv_vle8_v_u8m4,
  __riscv_vwsubu_vv_u16m8,
  __riscv_vse16_v_u16m8,
  vwsubu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vwsubu\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.vv\s+v24,v28,v16([^0-9]|$)} 1 } } */
