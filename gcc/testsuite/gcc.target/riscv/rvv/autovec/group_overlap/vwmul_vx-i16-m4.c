/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m4,
  vint16m4_t,
  vint32m8_t,
  int16_t,
  __riscv_vle16_v_i16m4,
  __riscv_vwmul_vx_i32m8,
  __riscv_vse32_v_i32m8,
  vwmul_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4)

/* { dg-final { scan-assembler-times {vwmul\.vx\s+v0,v4,} 1 } } */
/* { dg-final { scan-assembler-times {vwmul\.vx\s+v8,v12,} 1 } } */
