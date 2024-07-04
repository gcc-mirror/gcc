/* { dg-do compile } */
/* { dg-options "-mlittle-endian -mcpu=v4 -masm=normal" } */

unsigned short in16 = 0x1234U;
unsigned int   in32 = 0x12345678U;
unsigned long  in64 = 0x123456789abcdef0ULL;

unsigned short out16 = 0;
unsigned int   out32 = 0;
unsigned long  out64 = 0;

int foo (void)
{
  out16 = __builtin_bswap16 (in16);
  out32 = __builtin_bswap32 (in32);
  out64 = __builtin_bswap64 (in64);

  return 0;
}

/* { dg-final { scan-assembler "bswap\t%r., 16" } } */
/* { dg-final { scan-assembler "bswap\t%r., 32" } } */
/* { dg-final { scan-assembler "bswap\t%r., 64" } } */
