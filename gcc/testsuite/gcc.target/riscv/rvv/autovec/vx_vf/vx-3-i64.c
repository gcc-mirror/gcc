/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=15" } */

#include "vx_binary.h"

#define T int64_t

DEF_VX_BINARY_CASE_0_WRAP(T, +, add)
DEF_VX_BINARY_CASE_0_WRAP(T, -, sub)
DEF_VX_BINARY_REVERSE_CASE_0_WRAP(T, -, rsub)
DEF_VX_BINARY_CASE_0_WRAP(T, &, and)
DEF_VX_BINARY_CASE_0_WRAP(T, |, or)
DEF_VX_BINARY_CASE_0_WRAP(T, ^, xor)
DEF_VX_BINARY_CASE_0_WRAP(T, *, mul)
DEF_VX_BINARY_CASE_0_WRAP(T, /, div)
DEF_VX_BINARY_CASE_0_WRAP(T, %, rem)
DEF_VX_BINARY_CASE_2_WRAP(T, MAX_FUNC_0_WARP(T), max)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
/* { dg-final { scan-assembler-not {vrsub.vx} } } */
/* { dg-final { scan-assembler-not {vand.vx} } } */
/* { dg-final { scan-assembler-not {vor.vx} } } */
/* { dg-final { scan-assembler-not {vxor.vx} } } */
/* { dg-final { scan-assembler-not {vmul.vx} } } */
/* { dg-final { scan-assembler-not {vdiv.vx} } } */
/* { dg-final { scan-assembler-not {vrem.vx} } } */
/* { dg-final { scan-assembler-not {vmax.vx} } } */
