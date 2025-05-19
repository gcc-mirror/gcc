/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2" } */

#include "vx_binary.h"

DEF_VX_BINARY_CASE_1(uint8_t, +, add, VX_BINARY_BODY_X16)
DEF_VX_BINARY_CASE_1(uint8_t, -, sub, VX_BINARY_BODY_X16)

/* { dg-final { scan-assembler {vadd.vx} } } */
/* { dg-final { scan-assembler {vsub.vx} } } */
