/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e8m1,
  vuint8mf2_t,
  vuint16m1_t,
  uint8_t,
  __riscv_vle8_v_u8mf2,
  __riscv_vwaddu_vx_u16m1,
  __riscv_vse16_v_u16m1,
  vwaddu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-not {vwaddu\.vx\s+(v[0-9]+),\1,} } } */
