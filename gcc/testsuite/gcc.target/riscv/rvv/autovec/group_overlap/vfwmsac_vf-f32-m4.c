/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u32m8_f32m8 ( \
    __riscv_vreinterpret_v_u64m8_u32m8 (__riscv_vreinterpret_v_f64m8_u64m8 (v)))

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e32m4,
  vfloat32m4_t,
  vfloat64m8_t,
  vfloat32m8_t,
  float,
  __riscv_vle64_v_f64m8,
  RI_F,
  __riscv_vget_v_f32m8_f32m4,
  1,
  __riscv_vfwmsac_vf_f64m8,
  __riscv_vse64_v_f64m8,
  vfwmsac_vf,
  LOOP_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vfwmsac\.vf\s+v24,fa0,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vf\s+v16,fa0,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vf\s+v24,[a-z0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vf\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vfwmsac\.vf\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
