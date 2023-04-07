/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
#include <stdint-gcc.h>

int64_t foo1(int32_t rs1, int32_t rs2)
{
    return __builtin_riscv_pack(rs1, rs2);
}

int64_t foo2(int8_t rs1, int8_t rs2)
{
    return __builtin_riscv_packh(rs1, rs2);
}

int64_t foo3(int16_t rs1, int16_t rs2)
{
    return __builtin_riscv_packw(rs1, rs2);
}

int64_t foo4(int64_t rs1, int64_t rs2)
{
    return __builtin_riscv_brev8(rs1);
}
/* { dg-final { scan-assembler-times "pack\t" 1 } } */
/* { dg-final { scan-assembler-times "packh" 1 } } */
/* { dg-final { scan-assembler-times "packw" 1 } } */
/* { dg-final { scan-assembler-times "brev8" 1 } } */
