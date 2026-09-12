/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e8m8,
  vint8m8_t,
  vint16m1_t,
  vint16m8_t,
  __riscv_vle8_v_i8m8,
  __riscv_vreinterpret_v_i8m8_i16m8,
  __riscv_vget_v_i16m8_i16m1,
  7,
  __riscv_vwredsum_vs_i8m8_i16m1,
  __riscv_vse16_v_i16m1,
  vwredsum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v16,v16,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v8,v8,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v8,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v16,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
