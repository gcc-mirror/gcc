/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbkc -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
#include <stdint-gcc.h>

int32_t foo1(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_clmul(rs1, rs2);
}

int32_t foo2(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_clmulh(rs1, rs2);
}

/* { dg-final { scan-assembler-times "clmul\t" 1 } } */
/* { dg-final { scan-assembler-times "clmulh" 1 } } */
