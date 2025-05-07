/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_0(int16_t, +)

/* { dg-final { scan-assembler-times {vadd.vx} 1 } } */
