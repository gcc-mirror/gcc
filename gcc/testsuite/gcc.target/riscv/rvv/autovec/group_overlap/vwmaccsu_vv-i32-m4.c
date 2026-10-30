/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_UF(v) \
  __riscv_vreinterpret_v_u64m8_u32m8 (__riscv_vreinterpret_v_i64m8_u64m8 (v))

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m4,
  vint32m4_t,
  vuint32m4_t,
  vint64m8_t,
  vint32m8_t,
  vuint32m8_t,
  __riscv_vle32_v_i32m4,
  __riscv_vle32_v_u32m4,
  __riscv_vle64_v_i64m8,
  __riscv_vreinterpret_v_i64m8_i32m8,
  RI_UF,
  __riscv_vget_v_i32m8_i32m4,
  __riscv_vget_v_u32m8_u32m4,
  1,
  __riscv_vwmaccsu_vv_i64m8,
  __riscv_vse64_v_i64m8,
  vwmaccsu_vv,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v24,v20,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v16,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v24,v(24|25|26|27|28|29|30|31),} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v24,[a-z0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v16,v(16|17|18|19|20|21|22|23),} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v8,v(8|9|10|11|12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
