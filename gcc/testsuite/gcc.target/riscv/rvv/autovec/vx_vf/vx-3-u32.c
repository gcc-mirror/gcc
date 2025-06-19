/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=15" } */

#include "vx_binary.h"

#define T uint32_t

TEST_BINARY_VX_UNSIGNED_0(T)

/* { dg-final { scan-assembler-not {vadd.vx} } } */
/* { dg-final { scan-assembler-not {vsub.vx} } } */
/* { dg-final { scan-assembler-not {vrsub.vx} } } */
/* { dg-final { scan-assembler-not {vand.vx} } } */
/* { dg-final { scan-assembler-not {vor.vx} } } */
/* { dg-final { scan-assembler-not {vxor.vx} } } */
/* { dg-final { scan-assembler-not {vdivu.vx} } } */
/* { dg-final { scan-assembler-not {vremu.vx} } } */
/* { dg-final { scan-assembler-not {vmaxu.vx} } } */
/* { dg-final { scan-assembler-not {vminu.vx} } } */
