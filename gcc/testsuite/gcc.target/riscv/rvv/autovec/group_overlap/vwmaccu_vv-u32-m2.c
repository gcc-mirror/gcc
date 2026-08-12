/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vuint64m4_t,
  __riscv_vle32_v_u32m2,
  __riscv_vle64_v_u64m4,
  __riscv_vwmaccu_vv_u64m4,
  __riscv_vse64_v_u64m4,
  vwmaccu_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_X8)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vuint64m4_t,
  vuint32m4_t,
  __riscv_vle32_v_u32m2,
  __riscv_vle64_v_u64m4,
  __riscv_vreinterpret_v_u64m4_u32m4,
  __riscv_vget_v_u32m4_u32m2,
  __riscv_vwmaccu_vv_u64m4,
  __riscv_vse64_v_u64m4,
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
