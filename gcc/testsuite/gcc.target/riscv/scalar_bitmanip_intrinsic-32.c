/* { dg-do compile } */
/* { dg-require-effective-target rv32 } */
/* { dg-options "-march=rv32gc_zbb_zbc_zbkb_zbkc_zbkx -mabi=ilp32d" } */
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

uint32_t foo4 (uint32_t x)
{
    return __riscv_orc_b_32 (x);
}

uint32_t foo5 (uint32_t x, uint32_t shamt)
{
    return __riscv_ror_32 (x,shamt);
}

uint32_t foo6 (uint32_t x, uint32_t shamt)
{
    return __riscv_rol_32 (x,shamt);
}

uint32_t foo7 (uint32_t x)
{
    return __riscv_rev8_32 (x);
}

uint32_t foo8 (uint32_t x)
{
    return __riscv_brev8_32 (x);
}

uint32_t foo9 (uint32_t x)
{
    return __riscv_zip_32 (x);
}

uint32_t foo10 (uint32_t x)
{
    return __riscv_unzip_32 (x);
}

uint32_t foo11 (uint32_t rs1,uint32_t rs2)
{
    return __riscv_clmul_32 (rs1,rs2);
}

uint32_t foo12 (uint32_t rs1,uint32_t rs2)
{
    return __riscv_clmulh_32 (rs1,rs2);
}

uint32_t foo13 (uint32_t rs1,uint32_t rs2)
{
    return __riscv_clmulr_32 (rs1,rs2);
}

uint32_t foo14 (uint32_t rs1,uint32_t rs2)
{
    return __riscv_xperm4_32 (rs1,rs2);
}

uint32_t foo15 (uint32_t rs1,uint32_t rs2)
{
    return __riscv_xperm8_32 (rs1,rs2);
}

/* { dg-final { scan-assembler-times "clz" 1 } } */
/* { dg-final { scan-assembler-times "ctz" 1 } } */
/* { dg-final { scan-assembler-times "cpop" 1 } } */
/* { dg-final { scan-assembler-times "orc.b" 1 } } */
/* { dg-final { scan-assembler-times "ror" 1 } } */
/* { dg-final { scan-assembler-times "rol" 1 } } */
/* { dg-final { scan-assembler-times {\mrev8} 1 } } */
/* { dg-final { scan-assembler-times {\mbrev8} 1 } } */
/* { dg-final { scan-assembler-times {\mzip} 1 } } */
/* { dg-final { scan-assembler-times {\munzip} 1 } } */
/* { dg-final { scan-assembler-times "clmul\t" 1 } } */
/* { dg-final { scan-assembler-times "clmulh" 1 } } */
/* { dg-final { scan-assembler-times "clmulr" 1 } } */
/* { dg-final { scan-assembler-times "xperm4" 1 } } */
/* { dg-final { scan-assembler-times "xperm8" 1 } } */
