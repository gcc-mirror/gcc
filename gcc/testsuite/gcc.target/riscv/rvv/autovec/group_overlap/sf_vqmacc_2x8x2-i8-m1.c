/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvqmaccdod -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_i32m1_i8m1 (v)
#define RI_UF(v) \
  __riscv_vreinterpret_v_i32m1_i8m1 (v)
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m1,
  vint8m1_t,
  vint8m1_t,
  vint32m1_t,
  vint8m1_t,
  vint8m1_t,
  __riscv_vle8_v_i8m1,
  __riscv_vle8_v_i8m1,
  __riscv_vle32_v_i32m1,
  RI_F,
  RI_UF,
  GET_ID,
  GET_ID,
  1,
  __riscv_sf_vqmacc_2x8x2_i32m1,
  __riscv_vse32_v_i32m1,
  sf_vqmacc_2x8x2,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {sf\.vqmacc\.2x8x2\s+v3,v2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {sf\.vqmacc\.2x8x2\s+v0,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v3,v3,} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v3,[a-z0-9]+,v3([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v2,v2,} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v2,[a-z0-9]+,v2([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v1,v1,} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v1,[a-z0-9]+,v1([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v0,v0,} } } */
/* { dg-final { scan-assembler-not {sf\.vqmacc\.2x8x2\s+v0,[a-z0-9]+,v0([^0-9]|$)} } } */
