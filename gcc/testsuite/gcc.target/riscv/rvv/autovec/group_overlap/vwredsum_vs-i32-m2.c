/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e32m2,
  vint32m2_t,
  vint64m1_t,
  vint64m2_t,
  __riscv_vle32_v_i32m2,
  __riscv_vreinterpret_v_i32m2_i64m2,
  __riscv_vget_v_i64m2_i64m1,
  1,
  __riscv_vwredsum_vs_i32m2_i64m1,
  __riscv_vse64_v_i64m1,
  vwredsum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v4,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v2,v2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v2,v(2|3)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v4,v(4|5)([^0-9]|$)} } } */
