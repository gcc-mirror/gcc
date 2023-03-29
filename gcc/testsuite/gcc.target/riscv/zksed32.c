/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zksed -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

int32_t foo1(int32_t rs1, int32_t rs2, int bs)
{
    return __builtin_riscv_sm4ks(rs1,rs2,bs);
}

int32_t foo2(int32_t rs1, int32_t rs2, int bs)
{
    return __builtin_riscv_sm4ed(rs1,rs2,bs);
}


/* { dg-final { scan-assembler-times "sm4ks" 1 } } */
/* { dg-final { scan-assembler-times "sm4ed" 1 } } */
