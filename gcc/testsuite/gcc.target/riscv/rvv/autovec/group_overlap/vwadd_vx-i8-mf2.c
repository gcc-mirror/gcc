/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e8m1,
  vint8mf2_t,
  vint16m1_t,
  int8_t,
  __riscv_vle8_v_i8mf2,
  __riscv_vwadd_vx_i16m1,
  __riscv_vse16_v_i16m1,
  vwadd_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-not {vwadd\.vx\s+(v[0-9]+),\1,} } } */
