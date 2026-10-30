/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  vuint8m2_t,
  uint8_t,
  __riscv_vle16_v_u16m2,
  __riscv_vreinterpret_v_u16m2_u8m2,
  __riscv_vget_v_u8m2_u8m1,
  1,
  __riscv_vwmaccu_vx_u16m2,
  __riscv_vse16_v_u16m2,
  vwmaccu_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v6,a2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v4,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
