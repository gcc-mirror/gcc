/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc_zknd_zkne_zknh_zksed_zksh -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include "riscv_crypto.h"

uint64_t foo1 (uint64_t rs1, uint64_t rs2)
{
    return __riscv_aes64ds (rs1,rs2);
}

uint64_t foo2 (uint64_t rs1, uint64_t rs2)
{
    return __riscv_aes64dsm (rs1,rs2);
}

uint64_t foo3 (uint64_t rs1)
{
    return __riscv_aes64im (rs1);
}

uint64_t foo4 (uint64_t rs1)
{
    return __riscv_aes64ks1i (rs1,1);
}

uint64_t foo5 (uint64_t rs1, uint64_t rs2)
{
    return __riscv_aes64ks2 (rs1,rs2);
}

uint64_t foo6 (uint64_t rs1, uint64_t rs2)
{
    return __riscv_aes64es (rs1,rs2);
}

uint64_t foo7 (uint64_t rs1, uint64_t rs2)
{
    return __riscv_aes64esm (rs1,rs2);
}

uint64_t foo8 (uint64_t rs1)
{
    return __riscv_sha512sig0 (rs1);
}

uint64_t foo9 (uint64_t rs1)
{
    return __riscv_sha512sig1 (rs1);
}

uint64_t foo10 (uint64_t rs1)
{
    return __riscv_sha512sum0 (rs1);
}

uint64_t foo11 (uint64_t rs1)
{
    return __riscv_sha512sum1 (rs1);
}

uint32_t foo12 (uint32_t rs1)
{
    return __riscv_sha256sig0 (rs1);
}

uint32_t foo13 (uint32_t rs1)
{
    return __riscv_sha256sig1 (rs1);
}

uint32_t foo14 (uint32_t rs1)
{
    return __riscv_sha256sum0 (rs1);
}

uint32_t foo15 (uint32_t rs1)
{
    return __riscv_sha256sum1 (rs1);
}

uint32_t foo16 (uint32_t rs1)
{
    return __riscv_sm3p0 (rs1);
}

uint32_t foo17 (uint32_t rs1)
{
    return __riscv_sm3p1 (rs1);
}

uint32_t foo18 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sm4ed (rs1,rs2,1);
}

uint32_t foo19 (uint32_t rs1, uint32_t rs2)
{
    return __riscv_sm4ks (rs1,rs2,1);
}

/* { dg-final { scan-assembler-times "aes64ds\t" 1 } } */
/* { dg-final { scan-assembler-times "aes64dsm" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks1i" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks2" 1 } } */
/* { dg-final { scan-assembler-times "aes64im" 1 } } */
/* { dg-final { scan-assembler-times "aes64es\t" 1 } } */
/* { dg-final { scan-assembler-times "aes64esm" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks1i" 1 } } */
/* { dg-final { scan-assembler-times "aes64ks2" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig0" 1 } } */
/* { dg-final { scan-assembler-times "sha512sig1" 1 } } */
/* { dg-final { scan-assembler-times "sha512sum0" 1 } } */
/* { dg-final { scan-assembler-times "sha512sum1" 1 } } */
/* { dg-final { scan-assembler-times "sha256sig0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sig1" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum1" 1 } } */
/* { dg-final { scan-assembler-times "sm3p0" 1 } } */
/* { dg-final { scan-assembler-times "sm3p1" 1 } } */
/* { dg-final { scan-assembler-times "sm4ks" 1 } } */
/* { dg-final { scan-assembler-times "sm4ed" 1 } } */
