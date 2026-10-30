/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e8m4,
  vint8m4_t,
  vint16m8_t,
  vint8m8_t,
  uint8_t,
  __riscv_vle16_v_i16m8,
  __riscv_vreinterpret_v_i16m8_i8m8,
  __riscv_vget_v_i8m8_i8m4,
  1,
  __riscv_vwmaccus_vx_i16m8,
  __riscv_vse16_v_i16m8,
  vwmaccus_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v24,a2,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v16,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v24,[a-z0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
