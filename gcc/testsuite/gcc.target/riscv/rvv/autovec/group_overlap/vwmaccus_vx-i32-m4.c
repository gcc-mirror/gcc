/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e32m4,
  vint32m4_t,
  vint64m8_t,
  vint32m8_t,
  uint32_t,
  __riscv_vle64_v_i64m8,
  __riscv_vreinterpret_v_i64m8_i32m8,
  __riscv_vget_v_i32m8_i32m4,
  1,
  __riscv_vwmaccus_vx_i64m8,
  __riscv_vse64_v_i64m8,
  vwmaccus_vx,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v24,a2,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v16,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v24,[a-z0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
