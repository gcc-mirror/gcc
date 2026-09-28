/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m1,
  vfloat16mf4_t,
  vfloat32mf2_t,
  _Float16,
  __riscv_vle16_v_f16mf4,
  __riscv_vfwadd_vf_f32mf2,
  __riscv_vse32_v_f32mf2,
  vfwadd_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-not {vfwadd\.vf\s+(v[0-9]+),\1,} } } */
