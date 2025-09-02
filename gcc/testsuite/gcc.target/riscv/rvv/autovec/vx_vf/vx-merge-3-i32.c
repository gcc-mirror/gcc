/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=15" } */

#include "vx_binary.h"

#define T int32_t

DEF_VX_MERGE_0_WRAP(T)

/* { dg-final { scan-assembler-not {vmerge.vx} } } */
