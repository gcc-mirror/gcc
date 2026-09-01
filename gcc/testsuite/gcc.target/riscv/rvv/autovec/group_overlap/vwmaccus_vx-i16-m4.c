/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e16m4,
  vint16m4_t,
  vint32m8_t,
  uint16_t,
  __riscv_vle16_v_i16m4,
  __riscv_vle32_v_i32m8,
  __riscv_vwmaccus_vx_i32m8,
  __riscv_vse32_v_i32m8,
  vwmaccus_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X4)

DEF_GROUP_OVERLAP_TERNARY_5(
  __riscv_vsetvlmax_e16m4,
  vint16m4_t,
  vint32m8_t,
  vint16m8_t,
  uint16_t,
  __riscv_vle32_v_i32m8,
  __riscv_vreinterpret_v_i32m8_i16m8,
  __riscv_vget_v_i16m8_i16m4,
  __riscv_vwmaccus_vx_i32m8,
  __riscv_vse32_v_i32m8,
  vwmaccus_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_OVERLAP_X2)

/* ternary_4: the accumulator occupies the whole destination register group and
   is live when the narrowed source is read, so the source cannot be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v24,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v16,a2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v16,a2,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v8,a2,v4([^0-9]|$)} 1 } } */

/* ternary_5: the narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the source would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v16,a2,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccus\.vx\s+v8,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
