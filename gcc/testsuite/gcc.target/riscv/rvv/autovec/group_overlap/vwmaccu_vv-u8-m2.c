/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e8m2,
  vuint8m2_t,
  vuint16m4_t,
  __riscv_vle8_v_u8m2,
  __riscv_vle16_v_u16m4,
  __riscv_vwmaccu_vv_u16m4,
  __riscv_vse16_v_u16m4,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_X8)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e8m2,
  vuint8m2_t,
  vuint16m4_t,
  vuint8m4_t,
  __riscv_vle8_v_u8m2,
  __riscv_vle16_v_u16m4,
  __riscv_vreinterpret_v_u16m4_u8m4,
  __riscv_vget_v_u8m4_u8m2,
  __riscv_vwmaccu_vv_u16m4,
  __riscv_vse16_v_u16m4,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* ternary_0: the accumulator occupies the whole destination register group and
   is live when the narrowed sources are read, so no source can be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v8,v0,v30([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v0,v28,v26([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v28,v24,v22([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v24,v20,v18([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v20,v16,v14([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v16,v12,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v12,v8,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v8,v4,v2([^0-9]|$)} 1 } } */

/* ternary_1: each narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the sources would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v8,v10,v16([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v4,v6,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
