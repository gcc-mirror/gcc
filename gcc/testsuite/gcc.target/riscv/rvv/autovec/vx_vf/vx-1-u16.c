/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=gpr2vr-cost=0" } */

#include "vx_binary.h"
#include "vx_ternary.h"
#include "vx_widen.h"

#define T uint16_t
#define NT uint8_t

TEST_BINARY_VX_UNSIGNED_0(T)
TEST_TERNARY_VX_UNSIGNED_0(T)
TEST_WIDEN_BINARY_VX_UNSIGNED(T, NT)
TEST_WIDEN_TERNARY_VX_UNSIGNED(T, NT)

/* { dg-final { scan-assembler-times {vadd.vx} 2 } } */
/* { dg-final { scan-assembler-times {vsub.vx} 2 } } */
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
/* { dg-final { scan-assembler-times {vaaddu.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmacc.vx} 1 } } */
/* { dg-final { scan-assembler-times {vnmsac.vx} 1 } } */
/* { dg-final { scan-assembler-times {vmadd.vx} 1 } } */
/* { dg-final { scan-assembler-times {vnmsub.vx} 1 } } */
/* { dg-final { scan-assembler-not {vwaddu.vx} } } */
/* { dg-final { scan-assembler-not {vwsubu.vx} } } */
/* { dg-final { scan-assembler-not {vwmulu.vx} } } */
/* { dg-final { scan-assembler-not {vwaddu.wx} } } */
/* { dg-final { scan-assembler-not {vwsubu.wx} } } */
/* { dg-final { scan-assembler-not {vwmaccu.vx} } } */
