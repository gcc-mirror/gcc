/* { dg-do compile } */
/* { dg-require-effective-target rv32 } */
/* { dg-options "-march=rv32gc_zknd_zkne_zknh_zksed_zksh -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include "riscv_crypto.h"

uint32_t foo1 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_aes32dsi (rs1,rs2,1);
}

uint32_t foo2 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_aes32dsmi (rs1,rs2,1);
}

uint32_t foo3 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_aes32esi (rs1,rs2,1);
}

uint32_t foo4 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_aes32esmi (rs1,rs2,1);
}

uint32_t foo5 (uint32_t rs1)
{
    return __riscv_sha256sig0 (rs1);
}

uint32_t foo6 (uint32_t rs1)
{
    return __riscv_sha256sig1 (rs1);
}

uint32_t foo7 (uint32_t rs1)
{
    return __riscv_sha256sum0 (rs1);
}

uint32_t foo8 (uint32_t rs1)
{
    return __riscv_sha256sum1 (rs1);
}

uint32_t foo9 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sha512sig0h (rs1,rs2);
}

uint32_t foo10 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sha512sig0l (rs1,rs2);
}

uint32_t foo11 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sha512sig1h (rs1,rs2);
}

uint32_t foo12 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sha512sig1l (rs1,rs2);
}

uint32_t foo13 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sha512sum0r (rs1,rs2);
}

uint32_t foo14 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sha512sum1r (rs1,rs2);
}

uint32_t foo15 (uint32_t rs1)
{
    return __riscv_sm3p0 (rs1);
}

uint32_t foo16 (uint32_t rs1)
{
    return __riscv_sm3p1 (rs1);
}

uint32_t foo17 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sm4ed (rs1,rs2,1);
}

uint32_t foo18 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sm4ks (rs1,rs2,1);
}

/* { dg-final { scan-assembler-times "aes32dsi" 1 } } */
/* { dg-final { scan-assembler-times "aes32dsmi" 1 } } */
/* { dg-final { scan-assembler-times "aes32esi" 1 } } */
/* { dg-final { scan-assembler-times "aes32esmi" 1 } } */
/* { dg-final { scan-assembler-times "sha256sig0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sig1" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum1" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig0h" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig0l" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig1h" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig1l" 1 } } */
/* { dg-final { scan-assembler-times "sha512sum0r" 1 } } */
/* { dg-final { scan-assembler-times "sha512sum1r" 1 } } */
/* { dg-final { scan-assembler-times "sm3p0" 1 } } */
/* { dg-final { scan-assembler-times "sm3p1" 1 } } */
/* { dg-final { scan-assembler-times "sm4ks" 1 } } */
/* { dg-final { scan-assembler-times "sm4ed" 1 } } */
