/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" } } */

unsigned long zext64_32(unsigned int u32)
{
    return u32; //th.extu a0, a0, 31, 0
}

unsigned long zext64_16(unsigned short u16)
{
    return u16;
}

unsigned long zext64_8(unsigned char u8)
{
    return u8;
}

unsigned int zext32_64(unsigned long u64)
{
    return u64;
}

unsigned int zext32_16(unsigned short u16)
{
    return u16;
}

unsigned int zext32_8(unsigned char u8)
{
    return u8;
}

unsigned short zext16_64(unsigned long u64)
{
    return u64;
}

unsigned short zext16_32(unsigned int u32)
{
    return u32;
}

unsigned short zext16_8(unsigned char u8)
{
    return u8;
}

unsigned char zext8_64(unsigned long u64)
{
    return u64;
}

unsigned char zext8_32(unsigned int u32)
{
    return u32;
}

unsigned char zext8_16(unsigned short u16)
{
    return u16;
}

/* { dg-final { scan-assembler-not {\mslli} } } */
/* { dg-final { scan-assembler-not {\msrli} } } */
