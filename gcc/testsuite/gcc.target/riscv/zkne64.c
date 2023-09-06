/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zkne -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint64_t foo1(uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_aes64es(rs1,rs2);
}

uint64_t foo2(uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_aes64esm(rs1,rs2);
}

uint64_t foo3(uint64_t rs1, unsigned rnum)
{
    return __builtin_riscv_aes64ks1i(rs1,rnum);
}

uint64_t foo4(uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_aes64ks2(rs1,rs2);
}

/* { dg-final { scan-assembler-times "aes64es\t" 1 } } */
/* { dg-final { scan-assembler-times "aes64esm" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks1i" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks2" 1 } } */
