/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zksh -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

int64_t foo1(int64_t rs1)
{
    return __builtin_riscv_sm3p0(rs1);
}

int64_t foo2(int64_t rs1)
{
    return __builtin_riscv_sm3p1(rs1);
}


/* { dg-final { scan-assembler-times "sm3p0" 1 } } */
/* { dg-final { scan-assembler-times "sm3p1" 1 } } */
