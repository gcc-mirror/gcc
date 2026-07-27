/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e32m2,
  vint32m2_t,
  vint64m4_t,
  __riscv_vle32_v_i32m2,
  __riscv_vle64_v_i64m4,
  __riscv_vwsub_wv_i64m4,
  __riscv_vse64_v_i64m4,
  vwsub_wv,
  LOOP_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vwsub\.wv\s+v0,v8,v2} 1 } } */
/* { dg-final { scan-assembler-times {vwsub\.wv\s+v16,v16,v0} 1 } } */
