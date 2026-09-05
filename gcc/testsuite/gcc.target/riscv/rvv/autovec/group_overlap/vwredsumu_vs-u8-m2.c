/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_0(
  __riscv_vsetvlmax_e8m2,
  vuint8m2_t,
  vuint16m1_t,
  vuint16m2_t,
  __riscv_vle8_v_u8m2,
  __riscv_vreinterpret_v_u8m2_u16m2,
  __riscv_vget_v_u16m2_u16m1,
  1,
  __riscv_vwredsumu_vs_u8m2_u16m1,
  __riscv_vse16_v_u16m1,
  vwredsumu_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v4,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v2,v2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsumu\.vs\s+v[0-9]+,v2,v(2|3)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsumu\.vs\s+v[0-9]+,v4,v(4|5)([^0-9]|$)} } } */
