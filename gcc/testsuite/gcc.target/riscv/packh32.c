/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbkb -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" "-O0" } } */

#include <stdint-gcc.h>

int32_t foo1(int32_t rs1, int32_t rs2)
{
    return (rs1 & 255) | ((rs2 & 255) << 8);
}

/* { dg-final { scan-assembler-times "\\spackh\\s" 1 } } */

