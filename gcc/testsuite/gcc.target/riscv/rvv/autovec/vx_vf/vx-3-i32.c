/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=15" } */

#include "vx_binary.h"
#include "vx_ternary.h"

#define T int32_t

TEST_BINARY_VX_SIGNED_0(T)
TEST_TERNARY_VX_SIGNED_0(T)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
/* { dg-final { scan-assembler-not {vrsub.vx} } } */
/* { dg-final { scan-assembler-not {vand.vx} } } */
/* { dg-final { scan-assembler-not {vor.vx} } } */
/* { dg-final { scan-assembler-not {vxor.vx} } } */
/* { dg-final { scan-assembler-not {vmul.vx} } } */
/* { dg-final { scan-assembler-not {vdiv.vx} } } */
/* { dg-final { scan-assembler-not {vrem.vx} } } */
/* { dg-final { scan-assembler-not {vmax.vx} } } */
/* { dg-final { scan-assembler-not {vmin.vx} } } */
/* { dg-final { scan-assembler-not {vsadd.vx} } } */
/* { dg-final { scan-assembler-not {vssub.vx} } } */
/* { dg-final { scan-assembler-not {vaadd.vx} } } */
/* { dg-final { scan-assembler-not {vmacc.vx} } } */
/* { dg-final { scan-assembler-not {vnmsac.vx} } } */
/* { dg-final { scan-assembler-not {vmadd.vx} } } */
/* { dg-final { scan-assembler-not {vnmsub.vx} } } */
/* { dg-final { scan-assembler-not {vmseq.vx} } } */
/* { dg-final { scan-assembler-not {vmsne.vx} } } */
