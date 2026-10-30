/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvqmaccdod -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m2_u8m2 (__riscv_vreinterpret_v_i32m2_u32m2 (v))
#define RI_UF(v) \
  __riscv_vreinterpret_v_u32m2_u8m2 (__riscv_vreinterpret_v_i32m2_u32m2 (v))
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m2,
  vuint8m1_t,
  vuint8m2_t,
  vint32m2_t,
  vuint8m2_t,
  vuint8m2_t,
  __riscv_vle8_v_u8m1,
  __riscv_vle8_v_u8m2,
  __riscv_vle32_v_i32m2,
  RI_F,
  RI_UF,
  __riscv_vget_v_u8m2_u8m1,
  GET_ID,
  1,
  __riscv_sf_vqmaccu_2x8x2_i32m2,
  __riscv_vse32_v_i32m2,
  sf_vqmaccu_2x8x2,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {sf\.vqmaccu\.2x8x2\s+v6,v5,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {sf\.vqmaccu\.2x8x2\s+v4,v1,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.2x8x2\s+v6,v(6|7),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.2x8x2\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.2x8x2\s+v4,v(4|5),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.2x8x2\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.2x8x2\s+v2,v(2|3),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.2x8x2\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
