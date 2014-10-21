/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -mcrc32 -std=gnu89" } */
/* { dg-final { scan-assembler "__builtin_ia32_crc32di" } } */

unsigned long long
crc32d (unsigned long long x, unsigned long long y)
{
  return __builtin_ia32_crc32di (x, y);
}
