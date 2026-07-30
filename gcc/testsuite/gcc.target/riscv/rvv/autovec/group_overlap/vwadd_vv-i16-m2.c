/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e16m2,
  vint16m2_t,
  vint32m4_t,
  __riscv_vle16_v_i16m2,
  __riscv_vwadd_vv_i32m4,
  __riscv_vse32_v_i32m4,
  vwadd_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X8)

/* { dg-final { scan-assembler-times {vwadd\.vv\s+v8,v10,v6([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwadd\.vv\s+v28,v30,v24([^0-9]|$)} 1 } } */
