/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"

#define T uint64_t

TEST_BINARY_VX_UNSIGNED_0(T)

/* { dg-final { scan-assembler-times {vadd.vx} 1 } } */
/* { dg-final { scan-assembler-times {vsub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vrsub.vx} 1 } } */
/* { dg-final { scan-assembler-times {vand.vx} 1 } } */
/* { dg-final { scan-assembler-times {vor.vx} 1 } } */
/* { dg-final { scan-assembler-times {vxor.vx} 1 } } */
/* { dg-final { scan-assembler-times {vdivu.vx} 1 } } */
/* { dg-final { scan-assembler-times {vremu.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmaxu.vx} 2 } } */
/* { dg-final { scan-assembler-times {vminu.vx} 2 } } */
/* { dg-final { scan-assembler-times {vsaddu.vx} 1 } } */
/* { dg-final { scan-assembler-times {vssubu.vx} 1 } } */
