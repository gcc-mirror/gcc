/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xandesvdot -mabi=lp64d" } */

#include "andes_vector.h"
#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u64m2_u16m2 (v)
#define RI_UF(v) \
  __riscv_vreinterpret_v_u64m2_u16m2 (v)
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e64m2,
  vuint16m2_t,
  vuint16m2_t,
  vuint64m2_t,
  vuint16m2_t,
  vuint16m2_t,
  __riscv_vle16_v_u16m2,
  __riscv_vle16_v_u16m2,
  __riscv_vle64_v_u64m2,
  RI_F,
  RI_UF,
  GET_ID,
  GET_ID,
  1,
  __riscv_nds_vd4dotu_vv_u64m2,
  __riscv_vse64_v_u64m2,
  nds_vd4dotu_vv,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {nds\.vd4dotu\.vv\s+v6,v4,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {nds\.vd4dotu\.vv\s+v0,v8,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v6,v(6|7),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v6,[a-z0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v4,v(4|5),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v4,[a-z0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v2,v(2|3),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v2,[a-z0-9]+,v(2|3)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v0,v(0|1),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v0,[a-z0-9]+,v(0|1)([^0-9]|$)} } } */
