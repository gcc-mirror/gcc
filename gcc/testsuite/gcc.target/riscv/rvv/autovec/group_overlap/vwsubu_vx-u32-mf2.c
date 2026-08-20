/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_3(
  __riscv_vsetvlmax_e32m1,
  vuint32mf2_t,
  vuint64m1_t,
  uint32_t,
  __riscv_vle32_v_u32mf2,
  __riscv_vwsubu_vx_u64m1,
  __riscv_vse64_v_u64m1,
  vwsubu_vx,
  LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-not {vwsubu\.vx\s+(v[0-9]+),\1,} } } */
