/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zknh -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

int32_t foo1(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_sha512sig0h(rs1,rs2);
}

int32_t foo2(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_sha512sig0l(rs1,rs2);
}

int32_t foo3(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_sha512sig1h(rs1,rs2);
}

int32_t foo4(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_sha512sig1l(rs1,rs2);
}

int32_t foo5(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_sha512sum0r(rs1,rs2);
}

int32_t foo6(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_sha512sum1r(rs1,rs2);
}

/* { dg-final { scan-assembler-times "sha512sig0h" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig0l" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig1h" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig1l" 1 } } */
/* { dg-final { scan-assembler-times "sha512sum0r" 1 } } */
/* { dg-final { scan-assembler-times "sha512sum1r" 1 } } */
