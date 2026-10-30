/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xandesvdot -mabi=lp64d" } */

#include "andes_vector.h"
#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u64m4_u16m4 (v)
#define RI_UF(v) \
  __riscv_vreinterpret_v_u64m4_u16m4 (v)
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e64m4,
  vuint16m4_t,
  vuint16m4_t,
  vuint64m4_t,
  vuint16m4_t,
  vuint16m4_t,
  __riscv_vle16_v_u16m4,
  __riscv_vle16_v_u16m4,
  __riscv_vle64_v_u64m4,
  RI_F,
  RI_UF,
  GET_ID,
  GET_ID,
  1,
  __riscv_nds_vd4dotu_vv_u64m4,
  __riscv_vse64_v_u64m4,
  nds_vd4dotu_vv,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {nds\.vd4dotu\.vv\s+v12,v8,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {nds\.vd4dotu\.vv\s+v0,v16,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v12,[a-z0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v8,[a-z0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v4,[a-z0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v0,v(0|1|2|3),} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v0,[a-z0-9]+,v(0|1|2|3)([^0-9]|$)} } } */
