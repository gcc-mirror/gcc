/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc_zbb_zbc_zbkb_zbkc -mabi=lp64d " } */
/* { dg-skip-if "" { *-*-* } { "-g" "-O0" "-flto"} } */

#include "riscv_bitmanip.h"

uint32_t foo (uint32_t rs1)
{
    return __riscv_rev8_32 (rs1);
}

int32_t foo2(uint32_t rs1)
{
    return __riscv_brev8_32 (rs1);
}

uint32_t foo3 (uint32_t rs1)
{
    return __riscv_orc_b_32 (rs1);

}

uint32_t foo4 (uint32_t rs1)
{
    return __riscv_clmul_32 (rs1,rs1);
}

/* { dg-final { scan-assembler-times {\mrev8} 1 } } */
/* { dg-final { scan-assembler-times "brev8" 1 } } */
/* { dg-final { scan-assembler-times "clmul" 1 } } */
/* { dg-final { scan-assembler-times "orc.b" 1 } } */
/* { dg-final { scan-assembler-times "sext.w" 3 } } */
