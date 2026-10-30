/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e16m2,
  vuint16m2_t,
  vuint32m4_t,
  vuint16m4_t,
  uint16_t,
  __riscv_vle32_v_u32m4,
  __riscv_vreinterpret_v_u32m4_u16m4,
  __riscv_vget_v_u16m4_u16m2,
  1,
  __riscv_vwmaccu_vx_u32m4,
  __riscv_vse32_v_u32m4,
  vwmaccu_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v12,a2,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
