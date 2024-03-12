/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbkb -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint32_t foo1(uint16_t rs1, uint16_t rs2)
{
    return __builtin_riscv_pack(rs1, rs2);
}

uint32_t foo2(uint8_t rs1, uint8_t rs2)
{
    return __builtin_riscv_packh(rs1, rs2);
}

uint32_t foo3(uint32_t rs1)
{
    return __builtin_riscv_brev8(rs1);
}

uint32_t foo4(uint32_t rs1)
{
    return __builtin_riscv_zip(rs1);
}

uint32_t foo5(uint32_t rs1)
{
    return __builtin_riscv_unzip(rs1);
}

/* { dg-final { scan-assembler-times "pack\t" 1 } } */
/* { dg-final { scan-assembler-times {\mpackh} 1 } } */
/* { dg-final { scan-assembler-times {\mbrev8} 1 } } */
/* { dg-final { scan-assembler-times "\tzip\t" 1 } } */
/* { dg-final { scan-assembler-times {\munzip} 1 } } */
