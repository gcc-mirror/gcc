/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e8m2,
  vint8m2_t,
  vint16m4_t,
  vint8m4_t,
  __riscv_vle8_v_i8m2,
  __riscv_vle16_v_i16m4,
  __riscv_vreinterpret_v_i16m4_i8m4,
  __riscv_vget_v_i8m4_i8m2,
  1,
  __riscv_vwmacc_vv_i16m4,
  __riscv_vse16_v_i16m4,
  vwmacc_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmacc\.vv\s+v12,v10,v16([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmacc\.vv\s+v8,v2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vv\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
