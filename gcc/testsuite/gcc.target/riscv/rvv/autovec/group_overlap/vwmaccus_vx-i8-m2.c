/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e8m2,
  vint8m2_t,
  vint16m4_t,
  vint8m4_t,
  uint8_t,
  __riscv_vle16_v_i16m4,
  __riscv_vreinterpret_v_i16m4_i8m4,
  __riscv_vget_v_i8m4_i8m2,
  1,
  __riscv_vwmaccus_vx_i16m4,
  __riscv_vse16_v_i16m4,
  vwmaccus_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v12,a2,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v8,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
