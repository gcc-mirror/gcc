/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e32m1,
  vuint32m1_t,
  vuint64m2_t,
  __riscv_vle32_v_u32m1,
  __riscv_vwsubu_vv_u64m2,
  __riscv_vse64_v_u64m2,
  vwsubu_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* { dg-final { scan-assembler-times {vwsubu\.vv\s+v2,v1,v3([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vwsubu\.vv\s+v10,v11,v3([^0-9]|$)} 1 } } */
