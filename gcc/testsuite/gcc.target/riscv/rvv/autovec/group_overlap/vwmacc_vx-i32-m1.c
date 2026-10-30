/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e32m1,
  vint32m1_t,
  vint64m2_t,
  vint32m2_t,
  int32_t,
  __riscv_vle64_v_i64m2,
  __riscv_vreinterpret_v_i64m2_i32m2,
  __riscv_vget_v_i32m2_i32m1,
  1,
  __riscv_vwmacc_vx_i64m2,
  __riscv_vse64_v_i64m2,
  vwmacc_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmacc\.vx\s+v6,a2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmacc\.vx\s+v4,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmacc\.vx\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vx\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmacc\.vx\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
