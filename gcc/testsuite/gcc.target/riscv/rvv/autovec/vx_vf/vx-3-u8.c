/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=15" } */

#include "vx_binary.h"
#include "vx_ternary.h"

#define T uint8_t

TEST_BINARY_VX_UNSIGNED_0(T)
TEST_TERNARY_VX_UNSIGNED_0(T)

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
/* { dg-final { scan-assembler-not {vsaddu.vx} } } */
/* { dg-final { scan-assembler-not {vssubu.vx} } } */
/* { dg-final { scan-assembler-not {vaaddu.vx} } } */
/* { dg-final { scan-assembler-not {vmacc.vx} } } */
/* { dg-final { scan-assembler-not {vnmsac.vx} } } */
/* { dg-final { scan-assembler-not {vmadd.vx} } } */
/* { dg-final { scan-assembler-not {vnmsub.vx} } } */
/* { dg-final { scan-assembler-not {vmseq.vx} } } */
/* { dg-final { scan-assembler-not {vmsne.vx} } } */
/* { dg-final { scan-assembler-not {vmsltu.vx} } } */
/* { dg-final { scan-assembler-not {vmsleu.vx} } } */
