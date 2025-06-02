/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

#define T uint64_t

DEF_VX_BINARY_CASE_1_WRAP(T, +, add, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, -, sub, VX_BINARY_BODY)
DEF_VX_BINARY_REVERSE_CASE_1_WRAP(T, -, rsub, VX_BINARY_REVERSE_BODY);
DEF_VX_BINARY_CASE_1_WRAP(T, &, and, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, |, or, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1_WRAP(T, ^, xor, VX_BINARY_BODY)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
/* { dg-final { scan-assembler-not {vrsub.vx} } } */
/* { dg-final { scan-assembler-not {vand.vx} } } */
/* { dg-final { scan-assembler-not {vor.vx} } } */
/* { dg-final { scan-assembler-not {vxor.vx} } } */
