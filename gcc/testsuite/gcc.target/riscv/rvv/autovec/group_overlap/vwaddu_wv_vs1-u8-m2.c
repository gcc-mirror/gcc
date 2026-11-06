/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vuint8m2_t
pick1_u8m4_u8m2 (vuint16m4_t v)
{
  vuint8m4_t n = __riscv_vreinterpret_v_u16m4_u8m4 (v);

  return __riscv_vget_v_u8m4_u8m2 (n, 1);
}

static vuint8m2_t
pick0_u8m4_u8m2 (vuint16m4_t v)
{
  vuint8m4_t n = __riscv_vreinterpret_v_u16m4_u8m4 (v);

  return __riscv_vget_v_u8m4_u8m2 (n, 0);
}

DEF_GROUP_OVERLAP_BINARY_4(
  __riscv_vsetvlmax_e8m2,
  vuint8m2_t,
  vuint16m4_t,
  __riscv_vle16_v_u16m4,
  pick1_u8m4_u8m2,
  pick0_u8m4_u8m2,
  __riscv_vwaddu_wv_u16m4,
  __riscv_vse16_v_u16m4,
  vwaddu_wv_vs1,
  LOOP_WIDEN_BINARY_WV_BODY_OVERLAP_X2)

/* The vs1 is read with EEW = SEW while the vs2 is read with EEW = 2 * SEW,
   thus vs1 must not fall inside the vs2 register group.  */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v16,v12,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwaddu\.wv\s+v20,v0,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwaddu\.wv\s+v[0-9]+,(v0,v(0|1|2|3)|v4,v(4|5|6|7)|v8,v(8|9|10|11)|v12,v(12|13|14|15)|v16,v(16|17|18|19)|v20,v(20|21|22|23)|v24,v(24|25|26|27)|v28,v(28|29|30|31))([^0-9]|$)} } } */
