/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xandesvdot -mabi=lp64d" } */

#include "andes_vector.h"
#include "group_overlap.h"

#define RI_F(v) \
  __riscv_vreinterpret_v_u64m1_u16m1 (v)
#define RI_UF(v) \
  __riscv_vreinterpret_v_u64m1_u16m1 (v)
#define GET_ID(v, i) (v)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e64m1,
  vuint16m1_t,
  vuint16m1_t,
  vuint64m1_t,
  vuint16m1_t,
  vuint16m1_t,
  __riscv_vle16_v_u16m1,
  __riscv_vle16_v_u16m1,
  __riscv_vle64_v_u64m1,
  RI_F,
  RI_UF,
  GET_ID,
  GET_ID,
  1,
  __riscv_nds_vd4dotu_vv_u64m1,
  __riscv_vse64_v_u64m1,
  nds_vd4dotu_vv,
  LOOP_WIDEN_TERNARY_BODY_MIXED_OVERLAP_X2)

/* { dg-final { scan-assembler-times {nds\.vd4dotu\.vv\s+v3,v2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {nds\.vd4dotu\.vv\s+v0,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v3,v3,} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v3,[a-z0-9]+,v3([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v2,v2,} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v2,[a-z0-9]+,v2([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v1,v1,} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v1,[a-z0-9]+,v1([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v0,v0,} } } */
/* { dg-final { scan-assembler-not {nds\.vd4dotu\.vv\s+v0,[a-z0-9]+,v0([^0-9]|$)} } } */
