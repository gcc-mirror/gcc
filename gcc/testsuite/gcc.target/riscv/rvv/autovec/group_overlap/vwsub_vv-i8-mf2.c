/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_BINARY_1(
  __riscv_vsetvlmax_e8m1,
  vint8mf2_t,
  vint16m1_t,
  __riscv_vle8_v_i8mf2,
  __riscv_vwsub_vv_i16m1,
  __riscv_vse16_v_i16m1,
  vwsub_vv,
  LOOP_DUAL_WIDEN_BINARY_BODY_X16)

/* The fractional LMUL source has EMUL < 1, thus the widened destination
   register group must not overlap either source at all.  */
/* { dg-final { scan-assembler-not {vwsub\.vv\s+(v[0-9]+),\1,} } } */
/* { dg-final { scan-assembler-not {vwsub\.vv\s+(v[0-9]+),v[0-9]+,\1([^0-9]|$)} } } */
