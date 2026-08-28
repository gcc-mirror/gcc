/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e16m1,
  vuint16mf2_t,
  vint32m1_t,
  int16_t,
  __riscv_vle16_v_u16mf2,
  __riscv_vle32_v_i32m1,
  __riscv_vwmaccsu_vx_i32m1,
  __riscv_vse32_v_i32m1,
  vwmaccsu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx} 16 } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
