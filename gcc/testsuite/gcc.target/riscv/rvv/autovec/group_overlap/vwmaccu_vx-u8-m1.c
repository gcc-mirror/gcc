/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  uint8_t,
  __riscv_vle8_v_u8m1,
  __riscv_vle16_v_u16m2,
  __riscv_vwmaccu_vx_u16m2,
  __riscv_vse16_v_u16m2,
  vwmaccu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X16)

DEF_GROUP_OVERLAP_TERNARY_5(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vuint16m2_t,
  vuint8m2_t,
  uint8_t,
  __riscv_vle16_v_u16m2,
  __riscv_vreinterpret_v_u16m2_u8m2,
  __riscv_vget_v_u8m2_u8m1,
  __riscv_vwmaccu_vx_u16m2,
  __riscv_vse16_v_u16m2,
  vwmaccu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* ternary_4: the accumulator occupies the whole destination register group and
   is live when the narrowed source is read, so the source cannot be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v6,s0,v27([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v4,s0,v26([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v14,s0,v31([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v12,s0,v30([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v2,s0,v25([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v26,s0,v24([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v10,s0,v29([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v30,s0,v23([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v24,s0,v22([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,s0,v28([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v26,s0,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v16,s0,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v22,s0,v21([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v28,s0,v18([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v20,s0,v19([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v18,s0,v1([^0-9]|$)} 1 } } */

/* ternary_5: the narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the source would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v4,a2,v5([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v2,a2,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
