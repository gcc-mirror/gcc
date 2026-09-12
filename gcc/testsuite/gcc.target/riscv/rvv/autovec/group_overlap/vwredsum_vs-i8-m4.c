/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e8m4,
  vint8m4_t,
  vint16m1_t,
  vint16m4_t,
  __riscv_vle8_v_i8m4,
  __riscv_vreinterpret_v_i8m4_i16m4,
  __riscv_vget_v_i16m4_i16m1,
  3,
  __riscv_vwredsum_vs_i8m4_i16m1,
  __riscv_vse16_v_i16m1,
  vwredsum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v8,v8,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v4,v4,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v4,v(4|5|6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v8,v(8|9|10|11)([^0-9]|$)} } } */
