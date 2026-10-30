/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xandesvdot -mabi=lp64d" } */

#include "andes_vector.h"
#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_i32m8_i8m8 (v)
#define RI_UF(v) \
  __riscv_vreinterpret_v_u32m8_u8m8 (__riscv_vreinterpret_v_i32m8_u32m8 (v))
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m8,
  vint8m8_t,
  vuint8m8_t,
  vint32m8_t,
  vint8m8_t,
  vuint8m8_t,
  __riscv_vle8_v_i8m8,
  __riscv_vle8_v_u8m8,
  __riscv_vle32_v_i32m8,
  RI_F,
  RI_UF,
  GET_ID,
  GET_ID,
  1,
  __riscv_nds_vd4dotsu_vv_i32m8,
  __riscv_vse32_v_i32m8,
  nds_vd4dotsu_vv,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {nds\.vd4dotsu\.vv\s+v24,v16,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {nds\.vd4dotsu\.vv\s+v16,v24,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotsu\.vv\s+v24,v(24|25|26|27|28|29|30|31),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotsu\.vv\s+v24,[a-z0-9]+,v(24|25|26|27|28|29|30|31)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotsu\.vv\s+v16,v(16|17|18|19|20|21|22|23),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotsu\.vv\s+v16,[a-z0-9]+,v(16|17|18|19|20|21|22|23)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotsu\.vv\s+v8,v(8|9|10|11|12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotsu\.vv\s+v8,[a-z0-9]+,v(8|9|10|11|12|13|14|15)([^0-9]|$)} } } */
