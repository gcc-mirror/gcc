/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_UF(v) \
  __riscv_vreinterpret_v_u64m4_u32m4 (__riscv_vreinterpret_v_i64m4_u64m4 (v))

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m2,
  vint32m2_t,
  vuint32m2_t,
  vint64m4_t,
  vint32m4_t,
  vuint32m4_t,
  __riscv_vle32_v_i32m2,
  __riscv_vle32_v_u32m2,
  __riscv_vle64_v_i64m4,
  __riscv_vreinterpret_v_i64m4_i32m4,
  RI_UF,
  __riscv_vget_v_i32m4_i32m2,
  __riscv_vget_v_u32m4_u32m2,
  1,
  __riscv_vwmaccsu_vv_i64m4,
  __riscv_vse64_v_i64m4,
  vwmaccsu_vv,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v12,v10,v16([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v8,v2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
