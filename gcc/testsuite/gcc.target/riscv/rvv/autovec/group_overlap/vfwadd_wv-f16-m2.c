/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e16m2,
  vfloat16m2_t,
  vfloat32m4_t,
  __riscv_vle16_v_f16m2,
  __riscv_vle32_v_f32m4,
  __riscv_vfwadd_wv_f32m4,
  __riscv_vse32_v_f32m4,
  vfwadd_wv,
  LOOP_WIDEN_BINARY_BODY_X8)

/* The narrow source has EMUL == 2, thus the widened destination register
   group may overlap it in the highest-numbered part.  */
/* { dg-final { scan-assembler-times {vfwadd\.wv\s+v0,v8,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd\.wv\s+v16,v16,v0([^0-9]|$)} 1 } } */
