/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvqmaccqoq -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m4_u8m4 (__riscv_vreinterpret_v_i32m4_u32m4 (v))
#define RI_UF(v) \
  __riscv_vreinterpret_v_u32m4_u8m4 (__riscv_vreinterpret_v_i32m4_u32m4 (v))

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m4,
  vuint8m1_t,
  vuint8m2_t,
  vint32m4_t,
  vuint8m4_t,
  vuint8m4_t,
  __riscv_vle8_v_u8m1,
  __riscv_vle8_v_u8m2,
  __riscv_vle32_v_i32m4,
  RI_F,
  RI_UF,
  __riscv_vget_v_u8m4_u8m1,
  __riscv_vget_v_u8m4_u8m2,
  1,
  __riscv_sf_vqmaccu_4x8x4_i32m4,
  __riscv_vse32_v_i32m4,
  sf_vqmaccu_4x8x4,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {sf\.vqmaccu\.4x8x4\s+v12,v9,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {sf\.vqmaccu\.4x8x4\s+v8,v1,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.4x8x4\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.4x8x4\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.4x8x4\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.4x8x4\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.4x8x4\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccu\.4x8x4\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
