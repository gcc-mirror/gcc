/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=1" } */

#include "vx_binary.h"

#define T uint32_t

DEF_VX_BINARY_CASE_0_WRAP(T, +, add)
DEF_VX_BINARY_CASE_0_WRAP(T, -, sub)
DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, -, rsub)
DEF_VX_BINARY_CASE_0_WRAP(T, &, and)
DEF_VX_BINARY_CASE_0_WRAP(T, |, or)
DEF_VX_BINARY_CASE_0_WRAP(T, ^, xor)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
/* { dg-final { scan-assembler-not {vrsub.vx} } } */
/* { dg-final { scan-assembler-not {vand.vx} } } */
/* { dg-final { scan-assembler-not {vor.vx} } } */
/* { dg-final { scan-assembler-not {vxor.vx} } } */
