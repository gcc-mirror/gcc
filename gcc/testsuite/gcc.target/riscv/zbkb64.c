/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
#include <stdint-gcc.h>

uint64_t foo1(uint32_t rs1, uint32_t rs2)
{
    return __builtin_riscv_pack(rs1, rs2);
}

uint64_t foo2(uint8_t rs1, uint8_t rs2)
{
    return __builtin_riscv_packh(rs1, rs2);
}

uint64_t foo3(uint16_t rs1, uint16_t rs2)
{
    return __builtin_riscv_packw(rs1, rs2);
}

/* { dg-final { scan-assembler-times "pack\t" 1 } } */
/* { dg-final { scan-assembler-times {\mpackh} 1 } } */
/* { dg-final { scan-assembler-times {\mpackw} 1 } } */
