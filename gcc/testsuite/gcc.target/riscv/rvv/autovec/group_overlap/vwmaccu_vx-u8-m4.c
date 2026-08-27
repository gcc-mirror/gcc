/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e8m4,
  vuint8m4_t,
  vuint16m8_t,
  uint8_t,
  __riscv_vle8_v_u8m4,
  __riscv_vle16_v_u16m8,
  __riscv_vwmaccu_vx_u16m8,
  __riscv_vse16_v_u16m8,
  vwmaccu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X4)

DEF_GROUP_OVERLAP_TERNARY_5(
  __riscv_vsetvlmax_e8m4,
  vuint8m4_t,
  vuint16m8_t,
  vuint8m8_t,
  uint8_t,
  __riscv_vle16_v_u16m8,
  __riscv_vreinterpret_v_u16m8_u8m8,
  __riscv_vget_v_u8m8_u8m4,
  __riscv_vwmaccu_vx_u16m8,
  __riscv_vse16_v_u16m8,
  vwmaccu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* ternary_4: the accumulator occupies the whole destination register group and
   is live when the narrowed source is read, so the source cannot be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v24,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v16,a2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v16,a2,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,a2,v4([^0-9]|$)} 1 } } */

/* ternary_5: the narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the source would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v16,a2,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vx\s+v8,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
