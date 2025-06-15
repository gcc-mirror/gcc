/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"

#define T int64_t

TEST_BINARY_VX_SIGNED_0(T)

/* { dg-final { scan-assembler-times {vadd.vx} 1 } } */
/* { dg-final { scan-assembler-times {vsub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vrsub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vand.vx} 1 } } */
/* { dg-final { scan-assembler-times {vor.vx} 1 } } */
/* { dg-final { scan-assembler-times {vxor.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmul.vx} 1 } } */
/* { dg-final { scan-assembler-times {vdiv.vx} 1 } } */
/* { dg-final { scan-assembler-times {vrem.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmax.vx} 2 } } */
