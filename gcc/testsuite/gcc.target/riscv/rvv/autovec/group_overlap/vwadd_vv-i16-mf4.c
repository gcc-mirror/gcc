/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e16m1,
  vint16mf4_t,
  vint32mf2_t,
  __riscv_vle16_v_i16mf4,
  __riscv_vwadd_vv_i32mf2,
  __riscv_vse32_v_i32mf2,
  vwadd_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap either source at all.  */
/* { dg-final { scan-assembler-not {vwadd\.vv\s+(v[0-9]+),\1,} } } */
/* { dg-final { scan-assembler-not {vwadd\.vv\s+(v[0-9]+),v[0-9]+,\1([^0-9]|$)} } } */
