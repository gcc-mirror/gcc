/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zknd -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint64_t foo1(uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_aes64ds(rs1,rs2);
}

uint64_t foo2(uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_aes64dsm(rs1,rs2);
}

uint64_t foo3(uint64_t rs1, unsigned rnum)
{
    return __builtin_riscv_aes64ks1i(rs1,rnum);
}

uint64_t foo4(uint64_t rs1, uint64_t rs2)
{
    return __builtin_riscv_aes64ks2(rs1,rs2);
}

uint64_t foo5(uint64_t rs1)
{
    return __builtin_riscv_aes64im(rs1);
}

/* { dg-final { scan-assembler-times "aes64ds\t" 1 } } */
/* { dg-final { scan-assembler-times "aes64dsm" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks1i" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks2" 1 } } */
/* { dg-final { scan-assembler-times "aes64im" 1 } } */
