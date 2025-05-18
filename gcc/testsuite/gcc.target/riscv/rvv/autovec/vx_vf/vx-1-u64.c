/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_0(uint64_t, +, add)
DEF_VX_BINARY_CASE_0(uint64_t, -, sub)
DEF_VX_BINARY_REVERSE_CASE_0(uint64_t, -, rsub);

/* { dg-final { scan-assembler-times {vadd.vx} 1 } } */
/* { dg-final { scan-assembler-times {vsub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vrsub.vx} 1 } } */
