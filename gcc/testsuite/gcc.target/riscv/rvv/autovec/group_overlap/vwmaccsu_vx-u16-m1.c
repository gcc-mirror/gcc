/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vreinterpret_v_i32m2_u32m2 (v))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e16m1,
  vuint16m1_t,
  vint32m2_t,
  vuint16m2_t,
  int16_t,
  __riscv_vle32_v_i32m2,
  RI_F,
  __riscv_vget_v_u16m2_u16m1,
  1,
  __riscv_vwmaccsu_vx_i32m2,
  __riscv_vse32_v_i32m2,
  vwmaccsu_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v6,a2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v4,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
