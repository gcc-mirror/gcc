/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m4,
  vfloat16m4_t,
  vfloat32m8_t,
  _Float16,
  __riscv_vle16_v_f16m4,
  __riscv_vfwsub_vf_f32m8,
  __riscv_vse32_v_f32m8,
  vfwsub_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4)

/* { dg-final { scan-assembler-times {vfwsub\.vf\s+v0,v4,} 1 } } */
/* { dg-final { scan-assembler-times {vfwsub\.vf\s+v8,v12,} 1 } } */
