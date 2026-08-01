/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e16m2,
  vuint16m2_t,
  vuint32m4_t,
  __riscv_vle16_v_u16m2,
  __riscv_vwsubu_vv_u32m4,
  __riscv_vse32_v_u32m4,
  vwsubu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vwsubu\.vv\s+v8,v10,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.vv\s+v28,v30,v24([^0-9]|$)} 1 } } */
