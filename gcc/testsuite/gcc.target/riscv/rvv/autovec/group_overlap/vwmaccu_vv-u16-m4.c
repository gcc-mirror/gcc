/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vuint32m8_t,
  __riscv_vle16_v_u16m4,
  __riscv_vle32_v_u32m8,
  __riscv_vwmaccu_vv_u32m8,
  __riscv_vse32_v_u32m8,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_X4)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vuint32m8_t,
  vuint16m8_t,
  __riscv_vle16_v_u16m4,
  __riscv_vle32_v_u32m8,
  __riscv_vreinterpret_v_u32m8_u16m8,
  __riscv_vget_v_u16m8_u16m4,
  __riscv_vwmaccu_vv_u32m8,
  __riscv_vse32_v_u32m8,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* ternary_0: the accumulator occupies the whole destination register group and
   is live when the narrowed sources are read, so no source can be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v16,v0,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v0,v28,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v24,v16,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v16,v8,v12([^0-9]|$)} 1 } } */

/* ternary_1: each narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the sources would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v16,v20,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv\s+v8,v12,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
