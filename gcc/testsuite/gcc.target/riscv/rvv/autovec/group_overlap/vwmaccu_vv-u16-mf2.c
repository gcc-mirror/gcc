/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m1,
  vuint16mf2_t,
  vuint32m1_t,
  __riscv_vle16_v_u16mf2,
  __riscv_vle32_v_u32m1,
  __riscv_vwmaccu_vv_u32m1,
  __riscv_vse32_v_u32m1,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap either source at all.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vv} 16 } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+(v[0-9]+),\1,} } } */
/* { dg-final { scan-assembler-not {vwmaccu\.vv\s+(v[0-9]+),v[0-9]+,\1([^0-9]|$)} } } */
