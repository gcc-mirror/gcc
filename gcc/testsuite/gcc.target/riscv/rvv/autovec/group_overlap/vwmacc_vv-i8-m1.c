/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e8m1,
  vint8m1_t,
  vint16m2_t,
  vint8m2_t,
  __riscv_vle8_v_i8m1,
  __riscv_vle16_v_i16m2,
  __riscv_vreinterpret_v_i16m2_i8m2,
  __riscv_vget_v_i8m2_i8m1,
  1,
  __riscv_vwmacc_vv_i16m2,
  __riscv_vse16_v_i16m2,
  vwmacc_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmacc\.vv\s+v6,v5,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmacc\.vv\s+v4,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v6,v(6|7),} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v4,v(4|5),} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v2,v(2|3),} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
