/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse4.2 -mno-crc32" } */
/* { dg-final { scan-assembler "__builtin_ia32_crc32di" } } */

unsigned long long __builtin_ia32_crc32di (unsigned long long x,
					   unsigned long long y);

unsigned long long
crc32d (unsigned long long x, unsigned long long y)
{
  return __builtin_ia32_crc32di (x, y);
}
