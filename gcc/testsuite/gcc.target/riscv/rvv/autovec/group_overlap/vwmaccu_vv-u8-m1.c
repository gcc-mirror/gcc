/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  __riscv_vle8_v_u8m1,
  __riscv_vle16_v_u16m2,
  __riscv_vwmaccu_vv_u16m2,
  __riscv_vse16_v_u16m2,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_X16)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  vuint8m2_t,
  __riscv_vle8_v_u8m1,
  __riscv_vle16_v_u16m2,
  __riscv_vreinterpret_v_u16m2_u8m2,
  __riscv_vget_v_u8m2_u8m1,
  __riscv_vwmaccu_vv_u16m2,
  __riscv_vse16_v_u16m2,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* ternary_0: the accumulator occupies the whole destination register group and
   is live when the narrowed sources are read, so no source can be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v12,v30,v29([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v4,v16,v15([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v2,v0,v31([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v8,v14,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v30,v28,v27([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v28,v26,v25([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v6,v20,v19([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v26,v24,v23([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v2,v18,v17([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v10,v1,v15([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v24,v22,v21([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v14,v19,v17([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v16,v21,v23([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v18,v20,v22([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v20,v0,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v22,v0,v1([^0-9]|$)} 1 } } */

/* ternary_1: each narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the sources would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v4,v5,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v2,v3,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
