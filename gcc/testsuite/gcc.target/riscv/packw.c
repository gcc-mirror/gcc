/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" "-O0" } } */
#include <stdint-gcc.h>

uint32_t foo1(uint32_t rs1, uint32_t rs2)
{
    return (rs1 << 16) | ((rs2 << 16) >> 16);
}

/* { dg-final { scan-assembler-times "\\spackw\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\ssext\\s" } } */

