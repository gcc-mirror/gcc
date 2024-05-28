/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" "-O0" } } */
#include <stdint-gcc.h>

uint64_t foo1(uint64_t rs1, uint64_t rs2)
{
    return (rs1 << 32) | ((rs2 << 32) >> 32);
}

uint64_t foo2(uint64_t rs1, uint64_t rs2)
{
    return (rs1 << 32) | (rs2 & 4294967295);
}

/* { dg-final { scan-assembler-times "\\spack\\s" 2 } } */

