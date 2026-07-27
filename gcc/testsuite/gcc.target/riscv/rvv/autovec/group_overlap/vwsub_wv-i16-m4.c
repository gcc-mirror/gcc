/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e16m4,
  vint16m4_t,
  vint32m8_t,
  __riscv_vle16_v_i16m4,
  __riscv_vle32_v_i32m8,
  __riscv_vwsub_wv_i32m8,
  __riscv_vse32_v_i32m8,
  vwsub_wv,
  LOOP_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vwsub\.wv\s+v0,v0,v12} 1 } } */
/* { dg-final { scan-assembler-times {vwsub\.wv\s+v8,v8,v28} 1 } } */
