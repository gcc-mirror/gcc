/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m4,
  vfloat32m4_t,
  vfloat64m8_t,
  float,
  __riscv_vle32_v_f32m4,
  __riscv_vfwmul_vf_f64m8,
  __riscv_vse64_v_f64m8,
  vfwmul_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4)

/* { dg-final { scan-assembler-times {vfwmul\.vf\s+v0,v4,} 1 } } */
/* { dg-final { scan-assembler-times {vfwmul\.vf\s+v8,v12,} 1 } } */
