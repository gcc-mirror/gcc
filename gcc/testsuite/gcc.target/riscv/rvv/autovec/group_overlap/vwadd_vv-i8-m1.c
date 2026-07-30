/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e8m1,
  vint8m1_t,
  vint16m2_t,
  __riscv_vle8_v_i8m1,
  __riscv_vwadd_vv_i16m2,
  __riscv_vse16_v_i16m2,
  vwadd_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vwadd\.vv\s+v2,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwadd\.vv\s+v10,v11,v3([^0-9]|$)} 1 } } */
