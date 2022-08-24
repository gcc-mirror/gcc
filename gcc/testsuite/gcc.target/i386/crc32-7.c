/* { dg-do compile } */
/* { dg-options "-O2 -mcrc32" } */
/* { dg-final { scan-assembler "crc32b\[^\\n\]*eax" } } */
/* { dg-final { scan-assembler "crc32w\[^\\n\]*eax" } } */
/* { dg-final { scan-assembler "crc32l\[^\\n\]*eax" } } */
/* { dg-final { scan-assembler "crc32q\[^\\n\]*rax" { target { ! ia32 } } } } */

#include <immintrin.h>

unsigned int
test_mm_crc32_u8 (unsigned int CRC, unsigned char V)
{
  return _mm_crc32_u8 (CRC, V);
}

unsigned int
test_mm_crc32_u16 (unsigned int CRC, unsigned short V)
{
  return _mm_crc32_u16 (CRC, V);
}

unsigned int
test_mm_crc32_u32 (unsigned int CRC, unsigned int V)
{
  return _mm_crc32_u32 (CRC, V);
}

#ifdef __x86_64__
unsigned long long
test_mm_crc32_u64 (unsigned long long CRC, unsigned long long V)
{
  return _mm_crc32_u64 (CRC, V);
}
#endif
