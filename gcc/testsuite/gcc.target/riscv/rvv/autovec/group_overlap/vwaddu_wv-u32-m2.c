/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vuint64m4_t,
  __riscv_vle32_v_u32m2,
  __riscv_vle64_v_u64m4,
  __riscv_vwaddu_wv_u64m4,
  __riscv_vse64_v_u64m4,
  vwaddu_wv,
  LOOP_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v0,v8,v2} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v16,v16,v0} 1 } } */
