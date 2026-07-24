/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e8m2,
  vint8m2_t,
  vint16m4_t,
  __riscv_vle8_v_i8m2,
  __riscv_vle16_v_i16m4,
  __riscv_vwadd_wv_i16m4,
  __riscv_vse16_v_i16m4,
  vwadd_wv,
  LOOP_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vwadd\.wv\s+v0,v8,v2} 1 } } */
/* { dg-final { scan-assembler-times {vwadd\.wv\s+v16,v16,v0} 1 } } */
