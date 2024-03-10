/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbkx -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint32_t foo3(uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_xperm8(rs1, rs2);
}

uint32_t foo4(uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_xperm4(rs1, rs2);
}

/* { dg-final { scan-assembler-times "xperm8" 1 } } */
/* { dg-final { scan-assembler-times "xperm4" 1 } } */
