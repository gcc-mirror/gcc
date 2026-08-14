/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_2(
  __riscv_vsetvlmax_e8m4,
  vint8m4_t,
  vuint8m4_t,
  vint16m8_t,
  __riscv_vle8_v_i8m4,
  __riscv_vle8_v_u8m4,
  __riscv_vle16_v_i16m8,
  __riscv_vwmaccsu_vv_i16m8,
  __riscv_vse16_v_i16m8,
  vwmaccsu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_SU_X4)

DEF_GROUP_OVERLAP_TERNARY_3(
  __riscv_vsetvlmax_e8m4,
  vint8m4_t,
  vuint8m4_t,
  vint16m8_t,
  vint8m8_t,
  vuint8m8_t,
  __riscv_vle8_v_i8m4,
  __riscv_vle8_v_u8m4,
  __riscv_vle16_v_i16m8,
  __riscv_vreinterpret_v_i16m8_i8m8,
  __riscv_vreinterpret_v_i16m8_u16m8,
  __riscv_vreinterpret_v_u16m8_u8m8,
  __riscv_vget_v_i8m8_i8m4,
  __riscv_vget_v_u8m8_u8m4,
  __riscv_vwmaccsu_vv_i16m8,
  __riscv_vse16_v_i16m8,
  vwmaccsu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_SU_OVERLAP_X2)

/* ternary_2: the accumulator occupies the whole destination register group and
   is live when the narrowed sources are read, so no source can be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v16,v0,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v0,v28,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v24,v16,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v16,v8,v12([^0-9]|$)} 1 } } */

/* ternary_3: one narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  The signed source is the overlapping one in the first insn,
   the unsigned source in the second one.  Without the group overlap the
   sources would have to be copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v16,v20,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vv\s+v8,v4,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
