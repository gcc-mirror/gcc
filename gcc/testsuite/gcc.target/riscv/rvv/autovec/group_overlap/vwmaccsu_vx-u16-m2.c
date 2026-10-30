/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m4_u16m4 (__riscv_vreinterpret_v_i32m4_u32m4 (v))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e16m2,
  vuint16m2_t,
  vint32m4_t,
  vuint16m4_t,
  int16_t,
  __riscv_vle32_v_i32m4,
  RI_F,
  __riscv_vget_v_u16m4_u16m2,
  1,
  __riscv_vwmaccsu_vx_i32m4,
  __riscv_vse32_v_i32m4,
  vwmaccsu_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v12,a2,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v8,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
