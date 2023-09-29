/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zknd -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint32_t foo1(uint32_t rs1, uint32_t rs2, int bs)
{
    return __builtin_riscv_aes32dsi(rs1,rs2,bs);
}

uint32_t foo2(uint32_t rs1, uint32_t rs2, int bs)
{
    return __builtin_riscv_aes32dsmi(rs1,rs2,bs);
}

/* { dg-final { scan-assembler-times "aes32dsi" 1 } } */
/* { dg-final { scan-assembler-times "aes32dsmi" 1 } } */
