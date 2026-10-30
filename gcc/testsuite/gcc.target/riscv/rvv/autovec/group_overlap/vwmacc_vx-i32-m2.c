/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e32m2,
  vint32m2_t,
  vint64m4_t,
  vint32m4_t,
  int32_t,
  __riscv_vle64_v_i64m4,
  __riscv_vreinterpret_v_i64m4_i32m4,
  __riscv_vget_v_i32m4_i32m2,
  1,
  __riscv_vwmacc_vx_i64m4,
  __riscv_vse64_v_i64m4,
  vwmacc_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmacc\.vx\s+v12,a2,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmacc\.vx\s+v8,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmacc\.vx\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vx\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vx\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
