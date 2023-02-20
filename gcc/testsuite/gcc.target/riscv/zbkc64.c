/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbkc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
#include <stdint-gcc.h>

int64_t foo1(int64_t rs1, int64_t rs2)
{
    return __builtin_riscv_clmul(rs1, rs2);
}

int64_t foo2(int64_t rs1, int64_t rs2)
{
    return __builtin_riscv_clmulh(rs1, rs2);
}

/* { dg-final { scan-assembler-times "clmul\t" 1 } } */
/* { dg-final { scan-assembler-times "clmulh" 1 } } */
