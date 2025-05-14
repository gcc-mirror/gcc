
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_1(uint16_t, +, VX_BINARY_BODY_X8)

/* { dg-final { scan-assembler {vadd.vx} } } */
