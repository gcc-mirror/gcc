/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" } } */

long sext64_32(int s32)
{
    return s32;
}

long sext64_16(short s16)
{
    return s16;
}

long sext64_8(char s8)
{
    return s8;
}

int sext32_64(long s64)
{
    return s64;
}

int sext32_16(short s16)
{
    return s16;
}

int sext32_8(char s8)
{
    return s8;
}

short sext16_64(long s64)
{
    return s64;
}

short sext16_32(int s32)
{
    return s32;
}

short sext16_8(char s8)
{
    return s8;
}

char sext8_64(long s64)
{
    return s64;
}

char sext8_32(int s32)
{
    return s32;
}

char sext8_16(short s16)
{
    return s16;
}

/* { dg-final { scan-assembler-not {\mslli} } } */
/* { dg-final { scan-assembler-not {\msrli} } } */
