/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e32m1,
  vint32m1_t,
  vint64m2_t,
  __riscv_vle32_v_i32m1,
  __riscv_vle64_v_i64m2,
  __riscv_vwadd_wv_i64m2,
  __riscv_vse64_v_i64m2,
  vwadd_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vwadd\.wv\s+v0,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwadd\.wv\s+v2,v2,v25} 1 } } */
