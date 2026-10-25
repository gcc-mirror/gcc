/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

static inline vfloat16m8_t
reinterpret_f32m8_f16m8 (vfloat32m8_t v)
{
  return __riscv_vreinterpret_v_i16m8_f16m8 (
    __riscv_vreinterpret_v_i32m8_i16m8 (__riscv_vreinterpret_v_f32m8_i32m8 (v)));
}

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m4,
  vfloat16m4_t,
  vfloat32m8_t,
  __riscv_vle16_v_f16m4,
  __riscv_vle32_v_f32m8,
  __riscv_vfwmsac_vv_f32m8,
  __riscv_vse32_v_f32m8,
  vfwmsac_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_X4)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e16m4,
  vfloat16m4_t,
  vfloat32m8_t,
  vfloat16m8_t,
  __riscv_vle16_v_f16m4,
  __riscv_vle32_v_f32m8,
  reinterpret_f32m8_f16m8,
  __riscv_vget_v_f16m8_f16m4,
  __riscv_vfwmsac_vv_f32m8,
  __riscv_vse32_v_f32m8,
  vfwmsac_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* ternary_0: the accumulator occupies the whole destination register group and
   is live when the narrowed sources are read, so no source can be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v16,v0,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v0,v28,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v24,v16,v12([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v16,v8,v12([^0-9]|$)} 1 } } */

/* ternary_1: each narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the sources would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v16,v20,v0([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v8,v12,v4([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
