/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e32m1,
  vint32mf2_t,
  vuint32mf2_t,
  vint64m1_t,
  __riscv_vle32_v_i32mf2,
  __riscv_vle32_v_u32mf2,
  __riscv_vle64_v_i64m1,
  __riscv_vwmaccsu_vv_i64m1,
  __riscv_vse64_v_i64m1,
  vwmaccsu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_SU_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap either source at all.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv} 16 } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+(v[0-9]+),\1,} } } */
/* { dg-final { scan-assembler-not {vwmaccsu\.vv\s+(v[0-9]+),v[0-9]+,\1([^0-9]|$)} } } */
