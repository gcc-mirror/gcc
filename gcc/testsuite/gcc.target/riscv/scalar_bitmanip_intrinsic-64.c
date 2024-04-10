/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc_zbb_zbc_zbkb_zbkc_zbkx -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include "riscv_bitmanip.h"

unsigned foo1 (uint32_t x)
{
    return __riscv_clz_32 (x);
}

unsigned foo2 (uint32_t x)
{
    return __riscv_ctz_32 (x);
}

unsigned foo3 (uint32_t x)
{
    return __riscv_cpop_32 (x);
}

uint32_t foo4 (uint32_t x, uint8_t shamt)
{
    return __riscv_ror_32 (x,shamt);
}

uint32_t foo5 (uint32_t x, uint8_t shamt)
{
    return __riscv_rol_32 (x,shamt);
}

unsigned foo6 (uint64_t x)
{
    return __riscv_clz_64 (x);
}

unsigned foo7 (uint64_t x)
{
    return __riscv_ctz_64 (x);
}

unsigned foo8 (uint64_t x)
{
    return __riscv_cpop_64 (x);
}

uint64_t foo9 (uint64_t x)
{
    return __riscv_orc_b_64 (x);
}

uint64_t foo10 (uint64_t rs1, uint8_t rs2)
{
    return __riscv_ror_64 (rs1,rs2);
}

uint64_t foo11 (uint64_t rs1, uint8_t rs2)
{
    return __riscv_rol_64 (rs1,rs2);
}

uint64_t foo12 (uint64_t x)
{
    return __riscv_rev8_64 (x);
}

uint64_t foo13 (uint64_t x)
{
    return __riscv_brev8_64 (x);
}

uint64_t foo14 (uint64_t rs1,uint64_t rs2)
{
    return __riscv_clmul_64 (rs1,rs2);
}

uint64_t foo15 (uint64_t rs1,uint64_t rs2)
{
    return __riscv_clmulh_64 (rs1,rs2);
}

uint64_t foo16 (uint64_t rs1,uint64_t rs2)
{
    return __riscv_clmulr_64 (rs1,rs2);
}

uint64_t foo17 (uint64_t rs1,uint64_t rs2)
{
    return __riscv_xperm4_64 (rs1,rs2);
}

uint64_t foo18 (uint64_t rs1,uint64_t rs2)
{
    return __riscv_xperm8_64 (rs1,rs2);
}

/* { dg-final { scan-assembler-times "clzw" 1 } } */
/* { dg-final { scan-assembler-times "ctzw" 1 } } */
/* { dg-final { scan-assembler-times "cpopw" 1 } } */
/* { dg-final { scan-assembler-times "rorw" 1 } } */
/* { dg-final { scan-assembler-times "rolw" 1 } } */
/* { dg-final { scan-assembler-times "clz\t" 1 } } */
/* { dg-final { scan-assembler-times "ctz\t" 1 } } */
/* { dg-final { scan-assembler-times "cpop\t" 1 } } */
/* { dg-final { scan-assembler-times "orc.b" 1 } } */
/* { dg-final { scan-assembler-times "ror\t" 1 } } */
/* { dg-final { scan-assembler-times "rol\t" 1 } } */
/* { dg-final { scan-assembler-times {\mrev8} 1 } } */
/* { dg-final { scan-assembler-times {\mbrev8} 1 } } */
/* { dg-final { scan-assembler-times "clmul\t" 1 } } */
/* { dg-final { scan-assembler-times "clmulh" 1 } } */
/* { dg-final { scan-assembler-times "clmulr" 1 } } */
/* { dg-final { scan-assembler-times "xperm4" 1 } } */
/* { dg-final { scan-assembler-times "xperm8" 1 } } */
