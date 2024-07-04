/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbkb -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" "-O0" } } */

#include <stdint-gcc.h>

uint32_t foo1(uint32_t rs1, uint32_t rs2)
{
    return (rs1 << 16) | ((rs2 << 16) >> 16);
}

uint32_t foo2(uint32_t rs1, uint32_t rs2)
{
    return (rs1 << 16) | (rs2 & 65535);
}

/* { dg-final { scan-assembler-times "\\spack\\s" 2 } } */

