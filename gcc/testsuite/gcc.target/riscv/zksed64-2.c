/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zksed -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint32_t foo1(uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sm4ks(rs1,rs2,0);
}

uint32_t foo2(uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sm4ed(rs1,rs2,0);
}

uint32_t foo3(uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sm4ks(rs1,rs2,3);
}

uint32_t foo4(uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_sm4ed(rs1,rs2,3);
}


/* { dg-final { scan-assembler-times {\msm4ks} 2 } } */
/* { dg-final { scan-assembler-times {\msm4ed} 2 } } */
