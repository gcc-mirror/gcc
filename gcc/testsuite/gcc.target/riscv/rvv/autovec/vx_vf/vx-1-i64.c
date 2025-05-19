/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"

#define T int64_t

DEF_VX_BINARY_CASE_0_WRAP(T, +, add)
DEF_VX_BINARY_CASE_0_WRAP(T, -, sub)
DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, -, rsub);

/* { dg-final { scan-assembler-times {vadd.vx} 1 } } */
/* { dg-final { scan-assembler-times {vsub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vrsub.vx} 1 } } */
