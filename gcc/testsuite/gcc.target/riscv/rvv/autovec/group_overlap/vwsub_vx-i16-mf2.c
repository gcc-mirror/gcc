/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e16m1,
  vint16mf2_t,
  vint32m1_t,
  int16_t,
  __riscv_vle16_v_i16mf2,
  __riscv_vwsub_vx_i32m1,
  __riscv_vse32_v_i32m1,
  vwsub_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-not {vwsub\.vx\s+(v[0-9]+),\1,} } } */
