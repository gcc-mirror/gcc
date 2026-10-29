/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvabd -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m1,
  vuint16m1_t,
  vuint32m2_t,
  vuint16m2_t,
  __riscv_vle16_v_u16m1,
  __riscv_vle32_v_u32m2,
  __riscv_vreinterpret_v_u32m2_u16m2,
  __riscv_vget_v_u16m2_u16m1,
  1,
  __riscv_vwabdau_vv_u32m2,
  __riscv_vse32_v_u32m2,
  vwabdau_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwabdau\.vv\s+v6,v5,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwabdau\.vv\s+v4,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v6,v(6|7),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v6,v[0-9]+,v(6|7)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v4,v(4|5),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v4,v[0-9]+,v(4|5)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v2,v(2|3),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v2,v[0-9]+,v(2|3)([^0-9]|$)} } } */
