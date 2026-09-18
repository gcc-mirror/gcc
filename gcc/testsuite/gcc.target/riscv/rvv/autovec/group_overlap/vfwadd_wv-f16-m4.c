/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e16m4,
  vfloat16m4_t,
  vfloat32m8_t,
  __riscv_vle16_v_f16m4,
  __riscv_vle32_v_f32m8,
  __riscv_vfwadd_wv_f32m8,
  __riscv_vse32_v_f32m8,
  vfwadd_wv,
  LOOP_WIDEN_BINARY_BODY_X4)

/* The narrow source has EMUL == 4, thus the widened destination register
   group may overlap it in the highest-numbered part.  */
/* { dg-final { scan-assembler-times {vfwadd\.wv\s+v16,v0,v20([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd\.wv\s+v8,v8,v28([^0-9]|$)} 1 } } */
