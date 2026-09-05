/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_REDUCE_1(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m1_t,
  __riscv_vle8_v_u8m1,
  __riscv_vreinterpret_v_u8m1_u16m1,
  __riscv_vwredsumu_vs_u8m1_u16m1,
  __riscv_vse16_v_u16m1,
  vwredsumu_vs,
  LOOP_WIDEN_REDUCE_BODY_OVERLAP_LMUL1_X2)

/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v0,v2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwredsumu\.vs\s+v5,v1,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwredsumu\.vs\s+v[0-9]+,v1,v1([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwredsumu\.vs\s+v[0-9]+,v2,v2([^0-9]|$)} } } */
