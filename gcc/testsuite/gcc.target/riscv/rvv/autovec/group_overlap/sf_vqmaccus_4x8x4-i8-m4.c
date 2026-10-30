/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvqmaccqoq -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m8_u8m8 (__riscv_vreinterpret_v_i32m8_u32m8 (v))
#define RI_UF(v) \
  __riscv_vreinterpret_v_i32m8_i8m8 (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m8,
  vuint8m1_t,
  vint8m4_t,
  vint32m8_t,
  vuint8m8_t,
  vint8m8_t,
  __riscv_vle8_v_u8m1,
  __riscv_vle8_v_i8m4,
  __riscv_vle32_v_i32m8,
  RI_F,
  RI_UF,
  __riscv_vget_v_u8m8_u8m1,
  __riscv_vget_v_i8m8_i8m4,
  1,
  __riscv_sf_vqmaccus_4x8x4_i32m8,
  __riscv_vse32_v_i32m8,
  sf_vqmaccus_4x8x4,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {sf\.vqmaccus\.4x8x4\s+v24,v17,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {sf\.vqmaccus\.4x8x4\s+v16,v1,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccus\.4x8x4\s+v24,v(24|25|26|27|28|29|30|31),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccus\.4x8x4\s+v24,[a-z0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccus\.4x8x4\s+v16,v(16|17|18|19|20|21|22|23),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccus\.4x8x4\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccus\.4x8x4\s+v8,v(8|9|10|11|12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {sf\.vqmaccus\.4x8x4\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
