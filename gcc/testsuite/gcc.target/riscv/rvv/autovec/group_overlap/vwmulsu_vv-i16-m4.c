/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_2(
  __riscv_vsetvlmax_e16m4,
  vint16m4_t,
  vuint16m4_t,
  vint32m8_t,
  __riscv_vle16_v_i16m4,
  __riscv_vle16_v_u16m4,
  __riscv_vwmulsu_vv_i32m8,
  __riscv_vse32_v_i32m8,
  vwmulsu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_SU_X4)

/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v24,v28,v16([^0-9]|$)} 1 } } */
