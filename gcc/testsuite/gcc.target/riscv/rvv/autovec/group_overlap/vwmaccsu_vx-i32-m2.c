/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vint64m4_t,
  int32_t,
  __riscv_vle32_v_u32m2,
  __riscv_vle64_v_i64m4,
  __riscv_vwmaccsu_vx_i64m4,
  __riscv_vse64_v_i64m4,
  vwmaccsu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X8)

DEF_GROUP_OVERLAP_TERNARY_6(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vint64m4_t,
  vuint32m4_t,
  int32_t,
  __riscv_vle64_v_i64m4,
  __riscv_vreinterpret_v_i64m4_u64m4,
  __riscv_vreinterpret_v_u64m4_u32m4,
  __riscv_vget_v_u32m4_u32m2,
  __riscv_vwmaccsu_vx_i64m4,
  __riscv_vse64_v_i64m4,
  vwmaccsu_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_SU_OVERLAP_X2)

/* ternary_4: the accumulator occupies the whole destination register group and
   is live when the narrowed source is read, so the source cannot be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v12,a2,v30([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v8,a2,v28([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v28,a2,v24([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v4,a2,v26([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v16,a2,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v24,a2,v22([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v28,a2,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v20,a2,v2([^0-9]|$)} 1 } } */

/* ternary_6: the narrowed unsigned source is the highest-numbered half of its
   own signed accumulator, thus it overlaps the highest-numbered part of the
   destination register group.  Without the group overlap the source would have
   to be copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v8,a2,v10([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v4,a2,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
