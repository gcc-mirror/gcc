/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_0(
  __riscv_vsetvlmax_e32m1,
  vfloat32m1_t,
  vfloat64m2_t,
  __riscv_vle32_v_f32m1,
  __riscv_vle64_v_f64m2,
  __riscv_vfwsub_wv_f64m2,
  __riscv_vse64_v_f64m2,
  vfwsub_wv,
  LOOP_WIDEN_BINARY_BODY_X16)

/* The narrow source has EMUL == 1, thus the widened destination register
   group may overlap it in the highest-numbered part.  */
/* { dg-final { scan-assembler-times {vfwsub\.wv\s+v0,v4,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwsub\.wv\s+v18,v4,v19([^0-9]|$)} 1 } } */
