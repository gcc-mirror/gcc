/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_TERNARY_4(
  __riscv_vsetvlmax_e8m1,
  vint8mf8_t,
  vint16mf4_t,
  int8_t,
  __riscv_vle8_v_i8mf8,
  __riscv_vle16_v_i16mf4,
  __riscv_vwmacc_vx_i16mf4,
  __riscv_vse16_v_i16mf4,
  vwmacc_vx,
  LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap the source at all.  */
/* { dg-final { scan-assembler-times {vwmacc\.vx} 16 } } */
/* { dg-final { scan-assembler-not {vwmacc\.vx\s+(v[0-9]+),[a-z0-9]+,\1([^0-9]|$)} } } */
