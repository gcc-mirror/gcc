/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zksed -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint32_t foo1(uint32_t rs1, uint32_t rs2, unsigned bs)
{
    return __builtin_riscv_sm4ks(rs1,rs2,bs);
}

uint32_t foo2(uint32_t rs1, uint32_t rs2, unsigned bs)
{
    return __builtin_riscv_sm4ed(rs1,rs2,bs);
}


/* { dg-final { scan-assembler-times "sm4ks" 1 } } */
/* { dg-final { scan-assembler-times "sm4ed" 1 } } */
