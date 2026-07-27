/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vuint32m8_t,
  __riscv_vle16_v_u16m4,
  __riscv_vle32_v_u32m8,
  __riscv_vwsubu_wv_u32m8,
  __riscv_vse32_v_u32m8,
  vwsubu_wv,
  LOOP_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vwsubu\.wv\s+v0,v0,v12} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.wv\s+v8,v8,v28} 1 } } */
