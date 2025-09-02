/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"

#define T int16_t

DEF_VX_MERGE_0_WRAP(T)

/* { dg-final { scan-assembler-times {vmerge.vx} 1 } } */
