/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvqmaccdod -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_i32m4_i8m4 (v)
#define RI_UF(v) \
  __riscv_vreinterpret_v_i32m4_i8m4 (v)
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m4,
  vint8m1_t,
  vint8m4_t,
  vint32m4_t,
  vint8m4_t,
  vint8m4_t,
  __riscv_vle8_v_i8m1,
  __riscv_vle8_v_i8m4,
  __riscv_vle32_v_i32m4,
  RI_F,
  RI_UF,
  __riscv_vget_v_i8m4_i8m1,
  GET_ID,
  1,
  __riscv_sf_vqmacc_2x8x2_i32m4,
  __riscv_vse32_v_i32m4,
  sf_vqmacc_2x8x2,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {sf\.vqmacc\.2x8x2\s+v12,v9,v16([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {sf\.vqmacc\.2x8x2\s+v8,v1,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
