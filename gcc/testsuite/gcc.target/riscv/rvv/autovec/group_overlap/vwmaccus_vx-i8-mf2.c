/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e8m1,
  vint8mf2_t,
  vint16m1_t,
  uint8_t,
  __riscv_vle8_v_i8mf2,
  __riscv_vle16_v_i16m1,
  __riscv_vwmaccus_vx_i16m1,
  __riscv_vse16_v_i16m1,
  vwmaccus_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-times {vwmaccus\.vx} 16 } } */
/* { dg-final { scan-assembler-not {vwmaccus\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
