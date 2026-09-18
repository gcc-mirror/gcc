/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e16m1,
  vfloat16mf4_t,
  vfloat32mf2_t,
  __riscv_vle16_v_f16mf4,
  __riscv_vle32_v_f32mf2,
  __riscv_vfwadd_wv_f32mf2,
  __riscv_vse32_v_f32mf2,
  vfwadd_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* The narrow source has EMUL < 1, thus the widened destination register
   group must not overlap it at all.  */
/* { dg-final { scan-assembler-not {vfwadd\.wv\s+(v[0-9]+),v[0-9]+,\1([^0-9]|$)} } } */
