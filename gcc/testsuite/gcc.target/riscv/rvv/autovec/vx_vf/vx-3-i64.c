/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=15" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_0(int64_t, +, add)
DEF_VX_BINARY_CASE_0(int64_t, -, sub)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
