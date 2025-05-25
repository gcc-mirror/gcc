/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

#define T uint32_t

DEF_VX_BINARY_CASE_1_WRAP(T, +, add, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, -, sub, VX_BINARY_BODY_X4)
DEF_VX_BINARY_REVERSE_CASE_1_WRAP(T, -, rsub, VX_BINARY_REVERSE_BODY_X4);
DEF_VX_BINARY_CASE_1_WRAP(T, &, and, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, |, or, VX_BINARY_BODY_X4)
DEF_VX_BINARY_CASE_1_WRAP(T, ^, xor, VX_BINARY_BODY_X4)

/* { dg-final { scan-assembler {vadd.vx} } } */
/* { dg-final { scan-assembler {vsub.vx} } } */
/* { dg-final { scan-assembler {vrsub.vx} } } */
/* { dg-final { scan-assembler {vand.vx} } } */
/* { dg-final { scan-assembler {vor.vx} } } */
/* { dg-final { scan-assembler {vxor.vx} } } */
