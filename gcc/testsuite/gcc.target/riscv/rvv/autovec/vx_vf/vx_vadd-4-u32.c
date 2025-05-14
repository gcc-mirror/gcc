/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_1(uint32_t, +, VX_BINARY_BODY_X4)

/* { dg-final { scan-assembler {vadd.vx} } } */
