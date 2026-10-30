/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e8m1,
  vint8m1_t,
  vint16m2_t,
  vint8m2_t,
  uint8_t,
  __riscv_vle16_v_i16m2,
  __riscv_vreinterpret_v_i16m2_i8m2,
  __riscv_vget_v_i8m2_i8m1,
  1,
  __riscv_vwmaccus_vx_i16m2,
  __riscv_vse16_v_i16m2,
  vwmaccus_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v6,a2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v4,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
