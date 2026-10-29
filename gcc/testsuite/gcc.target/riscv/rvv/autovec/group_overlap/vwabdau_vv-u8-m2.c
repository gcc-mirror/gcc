/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvabd -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e8m2,
  vuint8m2_t,
  vuint16m4_t,
  vuint8m4_t,
  __riscv_vle8_v_u8m2,
  __riscv_vle16_v_u16m4,
  __riscv_vreinterpret_v_u16m4_u8m4,
  __riscv_vget_v_u8m4_u8m2,
  1,
  __riscv_vwabdau_vv_u16m4,
  __riscv_vse16_v_u16m4,
  vwabdau_vv,
  LOOP_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* { dg-final { scan-assembler-times {vwabdau\.vv\s+v12,v10,v16([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwabdau\.vv\s+v8,v2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v12,v(12|13|14|15),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v12,v[0-9]+,v(12|13|14|15)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v(8|9|10|11),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v[0-9]+,v(8|9|10|11)([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v4,v(4|5|6|7),} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v4,v[0-9]+,v(4|5|6|7)([^0-9]|$)} } } */
