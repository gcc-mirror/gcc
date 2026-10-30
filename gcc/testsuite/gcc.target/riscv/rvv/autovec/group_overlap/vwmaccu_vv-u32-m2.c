/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vuint64m4_t,
  vuint32m4_t,
  __riscv_vle32_v_u32m2,
  __riscv_vle64_v_u64m4,
  __riscv_vreinterpret_v_u64m4_u32m4,
  __riscv_vget_v_u32m4_u32m2,
  1,
  __riscv_vwmaccu_vv_u64m4,
  __riscv_vse64_v_u64m4,
  vwmaccu_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v12,v10,v16([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v8,v2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
