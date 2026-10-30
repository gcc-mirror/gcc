/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m8_u16m8 (__riscv_vreinterpret_v_i32m8_u32m8 (v))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vint32m8_t,
  vuint16m8_t,
  int16_t,
  __riscv_vle32_v_i32m8,
  RI_F,
  __riscv_vget_v_u16m8_u16m4,
  1,
  __riscv_vwmaccsu_vx_i32m8,
  __riscv_vse32_v_i32m8,
  vwmaccsu_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v24,a2,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v16,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v24,[a-z0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
