/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vint32m8_t,
  int16_t,
  __riscv_vle16_v_u16m4,
  __riscv_vle32_v_i32m8,
  __riscv_vwmaccsu_vx_i32m8,
  __riscv_vse32_v_i32m8,
  vwmaccsu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X4)

DEF_GROUP_OVERLAP_TERNARY_6(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vint32m8_t,
  vuint16m8_t,
  int16_t,
  __riscv_vle32_v_i32m8,
  __riscv_vreinterpret_v_i32m8_u32m8,
  __riscv_vreinterpret_v_u32m8_u16m8,
  __riscv_vget_v_u16m8_u16m4,
  __riscv_vwmaccsu_vx_i32m8,
  __riscv_vse32_v_i32m8,
  vwmaccsu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_SU_OVERLAP_X2)

/* ternary_4: the accumulator occupies the whole destination register group and
   is live when the narrowed source is read, so the source cannot be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v24,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v16,a2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v16,a2,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v8,a2,v4([^0-9]|$)} 1 } } */

/* ternary_6: the narrowed unsigned source is the highest-numbered half of its
   own signed accumulator, thus it overlaps the highest-numbered part of the
   destination register group.  Without the group overlap the source would have
   to be copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v16,a2,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v8,a2,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
