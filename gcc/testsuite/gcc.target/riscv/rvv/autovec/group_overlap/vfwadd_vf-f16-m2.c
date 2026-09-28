/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m2,
  vfloat16m2_t,
  vfloat32m4_t,
  _Float16,
  __riscv_vle16_v_f16m2,
  __riscv_vfwadd_vf_f32m4,
  __riscv_vse32_v_f32m4,
  vfwadd_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8)

/* { dg-final { scan-assembler-times {vfwadd\.vf\s+v0,v2,} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd\.vf\s+v4,v6,} 1 } } */
