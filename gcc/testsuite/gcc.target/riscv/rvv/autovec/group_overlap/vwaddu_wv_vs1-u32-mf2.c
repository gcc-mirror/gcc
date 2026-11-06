/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vuint32mf2_t
pick_u32m1_u32mf2 (vuint64m1_t v)
{
  vuint32m1_t n = __riscv_vreinterpret_v_u64m1_u32m1 (v);

  return __riscv_vlmul_trunc_v_u32m1_u32mf2 (n);
}

DEF_GROUP_OVERLAP_BINARY_4(
  __riscv_vsetvlmax_e32mf2,
  vuint32mf2_t,
  vuint64m1_t,
  __riscv_vle64_v_u64m1,
  pick_u32m1_u32mf2,
  pick_u32m1_u32mf2,
  __riscv_vwaddu_wv_u64m1,
  __riscv_vse64_v_u64m1,
  vwaddu_wv_vs1,
  LOOP_WIDEN_BINARY_WV_BODY_OVERLAP_X2)

/* The vs1 is read with EEW = SEW while the vs2 is read with EEW = 2 * SEW,
   thus vs1 must not fall inside the vs2 register group.  */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v4,v3,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v5,v0,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwaddu\.wv\s+v[0-9]+,(v[0-9]+),\1([^0-9]|$)} } } */
