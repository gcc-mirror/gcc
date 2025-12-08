/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"
#include "vx_ternary.h"

#define T int16_t

TEST_BINARY_VX_SIGNED_0(T)
TEST_TERNARY_VX_SIGNED_0(T)

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
/* { dg-final { scan-assembler-times {vmin.vx} 2 } } */
/* { dg-final { scan-assembler-times {vsadd.vx} 1 } } */
/* { dg-final { scan-assembler-times {vssub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vaadd.vx} 2 } } */
/* { dg-final { scan-assembler-times {vmacc.vx} 1 } } */
/* { dg-final { scan-assembler-times {vnmsac.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmadd.vx} 1 } } */
/* { dg-final { scan-assembler-times {vnmsub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmseq.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmsne.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmslt.vx} 1 } } */
