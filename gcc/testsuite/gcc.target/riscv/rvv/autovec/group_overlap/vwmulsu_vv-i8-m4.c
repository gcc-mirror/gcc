/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_2(
  __riscv_vsetvlmax_e8m4,
  vint8m4_t,
  vuint8m4_t,
  vint16m8_t,
  __riscv_vle8_v_i8m4,
  __riscv_vle8_v_u8m4,
  __riscv_vwmulsu_vv_i16m8,
  __riscv_vse16_v_i16m8,
  vwmulsu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_SU_X4)

/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmulsu\.vv\s+v24,v28,v16([^0-9]|$)} 1 } } */
