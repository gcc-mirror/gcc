/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e32m4,
  vint32m4_t,
  vint64m8_t,
  __riscv_vle32_v_i32m4,
  __riscv_vle64_v_i64m8,
  __riscv_vwadd_wv_i64m8,
  __riscv_vse64_v_i64m8,
  vwadd_wv,
  LOOP_WIDEN_BINARY_BODY_X4)

/* { dg-final { scan-assembler-times {vwadd\.wv\s+v0,v0,v12} 1 } } */
/* { dg-final { scan-assembler-times {vwadd\.wv\s+v8,v8,v28} 1 } } */
