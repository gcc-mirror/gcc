/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e32m1,
  vuint32m1_t,
  vuint64m2_t,
  vuint32m2_t,
  uint32_t,
  __riscv_vle64_v_u64m2,
  __riscv_vreinterpret_v_u64m2_u32m2,
  __riscv_vget_v_u32m2_u32m1,
  1,
  __riscv_vwmaccu_vx_u64m2,
  __riscv_vse64_v_u64m2,
  vwmaccu_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v6,a2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v4,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
