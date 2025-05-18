/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_1(uint64_t, +, add, VX_BINARY_BODY)
DEF_VX_BINARY_CASE_1(uint64_t, -, sub, VX_BINARY_BODY)
DEF_VX_BINARY_REVERSE_CASE_1(uint64_t, -, rsub, VX_BINARY_REVERSE_BODY);

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
/* { dg-final { scan-assembler-not {vrsub.vx} } } */
