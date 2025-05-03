/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=15" } */

#include "vx_binary.h"

DEF_VX_BINARY(int32_t, +)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
