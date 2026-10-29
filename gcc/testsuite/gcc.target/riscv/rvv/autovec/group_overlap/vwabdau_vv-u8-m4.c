/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvabd -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e8m4,
  vuint8m4_t,
  vuint16m8_t,
  vuint8m8_t,
  __riscv_vle8_v_u8m4,
  __riscv_vle16_v_u16m8,
  __riscv_vreinterpret_v_u16m8_u8m8,
  __riscv_vget_v_u8m8_u8m4,
  1,
  __riscv_vwabdau_vv_u16m8,
  __riscv_vse16_v_u16m8,
  vwabdau_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwabdau\.vv\s+v24,v20,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwabdau\.vv\s+v16,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v24,v(24|25|26|27|28|29|30|31),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v24,v[0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v16,v(16|17|18|19|20|21|22|23),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v16,v[0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v(8|9|10|11|12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v[0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
