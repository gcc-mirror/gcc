/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xandesvdot -mabi=lp64d" } */

#include "andes_vector.h"
#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_i64m8_i16m8 (v)
#define RI_UF(v) \
  __riscv_vreinterpret_v_u64m8_u16m8 (__riscv_vreinterpret_v_i64m8_u64m8 (v))
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e64m8,
  vint16m8_t,
  vuint16m8_t,
  vint64m8_t,
  vint16m8_t,
  vuint16m8_t,
  __riscv_vle16_v_i16m8,
  __riscv_vle16_v_u16m8,
  __riscv_vle64_v_i64m8,
  RI_F,
  RI_UF,
  GET_ID,
  GET_ID,
  1,
  __riscv_nds_vd4dotsu_vv_i64m8,
  __riscv_vse64_v_i64m8,
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
