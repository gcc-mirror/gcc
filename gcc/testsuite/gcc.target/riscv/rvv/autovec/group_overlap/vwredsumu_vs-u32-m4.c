/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e32m4,
  vuint32m4_t,
  vuint64m1_t,
  vuint64m4_t,
  __riscv_vle32_v_u32m4,
  __riscv_vreinterpret_v_u32m4_u64m4,
  __riscv_vget_v_u64m4_u64m1,
  3,
  __riscv_vwredsumu_vs_u32m4_u64m1,
  __riscv_vse64_v_u64m1,
  vwredsumu_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v8,v8,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v4,v4,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsumu\.vs\s+v[0-9]+,v4,v(4|5|6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsumu\.vs\s+v[0-9]+,v8,v(8|9|10|11)([^0-9]|$)} } } */
