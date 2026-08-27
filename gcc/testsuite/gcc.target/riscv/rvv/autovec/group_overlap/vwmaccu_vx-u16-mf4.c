/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e16m1,
  vuint16mf4_t,
  vuint32mf2_t,
  uint16_t,
  __riscv_vle16_v_u16mf4,
  __riscv_vle32_v_u32mf2,
  __riscv_vwmaccu_vx_u32mf2,
  __riscv_vse32_v_u32mf2,
  vwmaccu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vx} 16 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
