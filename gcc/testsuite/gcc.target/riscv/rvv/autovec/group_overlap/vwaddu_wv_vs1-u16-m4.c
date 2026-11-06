/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vuint16m4_t
pick1_u16m8_u16m4 (vuint32m8_t v)
{
  vuint16m8_t n = __riscv_vreinterpret_v_u32m8_u16m8 (v);

  return __riscv_vget_v_u16m8_u16m4 (n, 1);
}

static vuint16m4_t
pick0_u16m8_u16m4 (vuint32m8_t v)
{
  vuint16m8_t n = __riscv_vreinterpret_v_u32m8_u16m8 (v);

  return __riscv_vget_v_u16m8_u16m4 (n, 0);
}

DEF_GROUP_OVERLAP_BINARY_4(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vuint32m8_t,
  __riscv_vle32_v_u32m8,
  pick1_u16m8_u16m4,
  pick0_u16m8_u16m4,
  __riscv_vwaddu_wv_u32m8,
  __riscv_vse32_v_u32m8,
  vwaddu_wv_vs1,
  LOOP_WIDEN_BINARY_WV_BODY_OVERLAP_X2)

/* The vs1 is read with EEW = SEW while the vs2 is read with EEW = 2 * SEW,
   thus vs1 must not fall inside the vs2 register group.  */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v0,v24,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v24,v24,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwaddu\.wv\s+v[0-9]+,(v0,v(0|1|2|3|4|5|6|7)|v8,v(8|9|10|11|12|13|14|15)|v16,v(16|17|18|19|20|21|22|23)|v24,v(24|25|26|27|28|29|30|31))([^0-9]|$)} } } */
