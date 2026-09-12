/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_1(
  __riscv_vsetvlmax_e8m1,
  vint8m1_t,
  vint16m1_t,
  __riscv_vle8_v_i8m1,
  __riscv_vreinterpret_v_i8m1_i16m1,
  __riscv_vwredsum_vs_i8m1_i16m1,
  __riscv_vse16_v_i16m1,
  vwredsum_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_LMUL1_X2)

/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v0,v2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsum\.vs\s+v5,v1,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v1,v1([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsum\.vs\s+v[0-9]+,v2,v2([^0-9]|$)} } } */
