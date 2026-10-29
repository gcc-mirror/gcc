/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvabd -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_i32m8_i16m8 (__riscv_vreinterpret_v_u32m8_i32m8 (v))

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m4,
  vint16m4_t,
  vuint32m8_t,
  vint16m8_t,
  __riscv_vle16_v_i16m4,
  __riscv_vle32_v_u32m8,
  RI_F,
  __riscv_vget_v_i16m8_i16m4,
  1,
  __riscv_vwabda_vv_u32m8,
  __riscv_vse32_v_u32m8,
  vwabda_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwabda\.vv\s+v24,v20,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwabda\.vv\s+v16,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v24,v(24|25|26|27|28|29|30|31),} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v24,v[0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v16,v(16|17|18|19|20|21|22|23),} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v16,v[0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v(8|9|10|11|12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v[0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
