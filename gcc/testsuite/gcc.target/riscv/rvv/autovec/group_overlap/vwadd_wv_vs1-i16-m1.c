/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

static vint16m1_t
pick1_i16m2_i16m1 (vint32m2_t v)
{
  vint16m2_t n = __riscv_vreinterpret_v_i32m2_i16m2 (v);

  return __riscv_vget_v_i16m2_i16m1 (n, 1);
}

static vint16m1_t
pick0_i16m2_i16m1 (vint32m2_t v)
{
  vint16m2_t n = __riscv_vreinterpret_v_i32m2_i16m2 (v);

  return __riscv_vget_v_i16m2_i16m1 (n, 0);
}

DEF_GROUP_OVERLAP_BINARY_4(
  __riscv_vsetvlmax_e16m1,
  vint16m1_t,
  vint32m2_t,
  __riscv_vle32_v_i32m2,
  pick1_i16m2_i16m1,
  pick0_i16m2_i16m1,
  __riscv_vwadd_wv_i32m2,
  __riscv_vse32_v_i32m2,
  vwadd_wv_vs1,
  LOOP_WIDEN_BINARY_WV_BODY_OVERLAP_X2)

/* The vs1 is read with EEW = SEW while the vs2 is read with EEW = 2 * SEW,
   thus vs1 must not fall inside the vs2 register group.  */
/* { dg-final { scan-assembler-times {vwadd\.wv\s+v10,v0,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwadd\.wv\s+v8,v6,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwadd\.wv\s+v[0-9]+,(v0,v(0|1)|v2,v(2|3)|v4,v(4|5)|v6,v(6|7)|v8,v(8|9)|v10,v(10|11)|v12,v(12|13)|v14,v(14|15)|v16,v(16|17)|v18,v(18|19)|v20,v(20|21)|v22,v(22|23)|v24,v(24|25)|v26,v(26|27)|v28,v(28|29)|v30,v(30|31))([^0-9]|$)} } } */
