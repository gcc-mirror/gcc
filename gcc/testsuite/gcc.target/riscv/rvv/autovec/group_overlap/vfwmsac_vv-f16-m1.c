/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

static inline vfloat16m2_t
reinterpret_f32m2_f16m2 (vfloat32m2_t v)
{
  return __riscv_vreinterpret_v_i16m2_f16m2 (
    __riscv_vreinterpret_v_i32m2_i16m2 (__riscv_vreinterpret_v_f32m2_i32m2 (v)));
}

DEF_GROUP_OVERLAP_TERNARY_0(
  __riscv_vsetvlmax_e16m1,
  vfloat16m1_t,
  vfloat32m2_t,
  __riscv_vle16_v_f16m1,
  __riscv_vle32_v_f32m2,
  __riscv_vfwmsac_vv_f32m2,
  __riscv_vse32_v_f32m2,
  vfwmsac_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_X16)

DEF_GROUP_OVERLAP_TERNARY_1(
  __riscv_vsetvlmax_e16m1,
  vfloat16m1_t,
  vfloat32m2_t,
  vfloat16m2_t,
  __riscv_vle16_v_f16m1,
  __riscv_vle32_v_f32m2,
  reinterpret_f32m2_f16m2,
  __riscv_vget_v_f16m2_f16m1,
  __riscv_vfwmsac_vv_f32m2,
  __riscv_vse32_v_f32m2,
  vfwmsac_vv,
  LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2)

/* ternary_0: the accumulator occupies the whole destination register group and
   is live when the narrowed sources are read, so no source can be allocated
   inside the destination register group.  */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v12,v30,v29([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v4,v16,v15([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v2,v0,v31([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v8,v14,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v30,v28,v27([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v28,v26,v25([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v6,v20,v19([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v26,v24,v23([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v2,v18,v17([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v10,v1,v15([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v24,v22,v21([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v14,v19,v17([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v16,v21,v23([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v18,v20,v22([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v20,v0,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v22,v0,v1([^0-9]|$)} 1 } } */

/* ternary_1: each narrowed source is the highest-numbered half of its own
   accumulator, thus it overlaps the highest-numbered part of the destination
   register group.  Without the group overlap the sources would have to be
   copied out to a disjoint register group first.  */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v4,v5,v8([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.vv\s+v2,v3,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-not {vmv[0-9]+r\.v} } } */
