/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m1,
  vfloat16m1_t,
  vfloat32m2_t,
  _Float16,
  __riscv_vle16_v_f16m1,
  __riscv_vfwadd_vf_f32m2,
  __riscv_vse32_v_f32m2,
  vfwadd_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* { dg-final { scan-assembler-times {vfwadd\.vf\s+v0,v1,} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd\.vf\s+v2,v3,} 1 } } */
