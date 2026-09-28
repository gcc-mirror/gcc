/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m1,
  vfloat32mf2_t,
  vfloat64m1_t,
  float,
  __riscv_vle32_v_f32mf2,
  __riscv_vfwsub_vf_f64m1,
  __riscv_vse64_v_f64m1,
  vfwsub_vf,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-not {vfwsub\.vf\s+(v[0-9]+),\1,} } } */
