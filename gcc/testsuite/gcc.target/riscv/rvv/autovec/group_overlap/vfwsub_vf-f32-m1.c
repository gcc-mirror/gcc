/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m1,
  vfloat32m1_t,
  vfloat64m2_t,
  float,
  __riscv_vle32_v_f32m1,
  __riscv_vfwsub_vf_f64m2,
  __riscv_vse64_v_f64m2,
  vfwsub_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* { dg-final { scan-assembler-times {vfwsub\.vf\s+v0,v1,} 1 } } */
/* { dg-final { scan-assembler-times {vfwsub\.vf\s+v2,v3,} 1 } } */
