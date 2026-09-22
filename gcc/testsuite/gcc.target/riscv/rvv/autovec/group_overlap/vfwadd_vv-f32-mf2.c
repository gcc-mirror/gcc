/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e32m1,
  vfloat32mf2_t,
  vfloat64m1_t,
  __riscv_vle32_v_f32mf2,
  __riscv_vfwadd_vv_f64m1,
  __riscv_vse64_v_f64m1,
  vfwadd_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap either source at all.  */
/* { dg-final { scan-assembler-not {vfwadd\.vv\s+(v[0-9]+),\1,} } } */
/* { dg-final { scan-assembler-not {vfwadd\.vv\s+(v[0-9]+),v[0-9]+,\1([^0-9]|$)} } } */
