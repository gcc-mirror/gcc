/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m2,
  vfloat32m2_t,
  vfloat64m4_t,
  float,
  __riscv_vle32_v_f32m2,
  __riscv_vfwsub_vf_f64m4,
  __riscv_vse64_v_f64m4,
  vfwsub_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8)

/* { dg-final { scan-assembler-times {vfwsub\.vf\s+v0,v2,} 1 } } */
/* { dg-final { scan-assembler-times {vfwsub\.vf\s+v4,v6,} 1 } } */
