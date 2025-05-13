/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=1" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_1(uint64_t, +, add, VX_BINARY_BODY)

/* { dg-final { scan-assembler {vadd.vx} } } */
