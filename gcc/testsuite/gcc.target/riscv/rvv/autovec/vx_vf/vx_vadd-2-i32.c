/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=1" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_0(int32_t, +)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
