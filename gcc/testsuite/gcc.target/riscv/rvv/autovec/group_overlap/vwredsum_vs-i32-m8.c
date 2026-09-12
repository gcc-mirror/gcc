/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e32m8,
  vint32m8_t,
  vint64m1_t,
  vint64m8_t,
  __riscv_vle32_v_i32m8,
  __riscv_vreinterpret_v_i32m8_i64m8,
  __riscv_vget_v_i64m8_i64m1,
  7,
  __riscv_vwredsum_vs_i32m8_i64m1,
  __riscv_vse64_v_i64m1,
  vwredsum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v16,v16,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v8,v8,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v8,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v16,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
