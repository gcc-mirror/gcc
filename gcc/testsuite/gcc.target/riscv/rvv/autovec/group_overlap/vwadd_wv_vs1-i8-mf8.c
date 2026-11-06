/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vint8mf8_t
pick_i8mf4_i8mf8 (vint16mf4_t v)
{
  vint8mf4_t n = __riscv_vreinterpret_v_i16mf4_i8mf4 (v);

  return __riscv_vlmul_trunc_v_i8mf4_i8mf8 (n);
}

DEF_GROUP_OVERLAP_BINARY_4(
  __riscv_vsetvlmax_e8mf8,
  vint8mf8_t,
  vint16mf4_t,
  __riscv_vle16_v_i16mf4,
  pick_i8mf4_i8mf8,
  pick_i8mf4_i8mf8,
  __riscv_vwadd_wv_i16mf4,
  __riscv_vse16_v_i16mf4,
  vwadd_wv_vs1,
  LOOP_WIDEN_BINARY_WV_BODY_OVERLAP_X2)

/* The vs1 is read with EEW = SEW while the vs2 is read with EEW = 2 * SEW,
   thus vs1 must not fall inside the vs2 register group.  */
/* { dg-final { scan-assembler-times {vwadd\.wv\s+v4,v3,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwadd\.wv\s+v5,v0,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v[0-9]+,(v[0-9]+),\1([^0-9]|$)} } } */
