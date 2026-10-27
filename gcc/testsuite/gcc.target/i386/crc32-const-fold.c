/* { dg-do compile } */
/* { dg-options "-O2 -mcrc32" } */

#include <immintrin.h>

unsigned int
test_u8_1 ()
{
  return _mm_crc32_u8 (0x02a24bf8, 0xba);
}

unsigned int
test_u8_2 ()
{
  return _mm_crc32_u8 (0x0cd5e10f, 0xe7);
}

unsigned int
test_u8_3 ()
{
  return _mm_crc32_u8 (0x50e739b8, 0x2e);
}

unsigned int
test_u8_4 ()
{
  return _mm_crc32_u8 (0x3c2db116, 0x3d);
}

unsigned int
test_u8_5 ()
{
  return _mm_crc32_u8 (-1, -1);
}

unsigned int
test_u16_1 ()
{
  return _mm_crc32_u16 (0x02a24bf8, 0xd4ba);
}

unsigned int
test_u16_2 ()
{
  return _mm_crc32_u16 (0x0cd5e10f, 0x42e7);
}

unsigned int
test_u16_3 ()
{
  return _mm_crc32_u16 (0x50e739b8, 0x382e);
}

unsigned int
test_u16_4 ()
{
  return _mm_crc32_u16 (0x3c2db116, 0x7f3d);
}

unsigned int
test_u16_5 ()
{
  return _mm_crc32_u16 (-1, -1);
}

unsigned int
test_u32_1 ()
{
  return _mm_crc32_u32 (0x02a24bf8, 0xf37dd4ba);
}

unsigned int
test_u32_2 ()
{
  return _mm_crc32_u32 (0x0cd5e10f, 0x832b42e7);
}

unsigned int
test_u32_3 ()
{
  return _mm_crc32_u32 (0x50e739b8, 0xcefb382e);
}

unsigned int
test_u32_4 ()
{
  return _mm_crc32_u32 (0x3c2db116, 0xd3947f3d);
}

unsigned int
test_u32_5 ()
{
  return _mm_crc32_u32 (-1, -1);
}

#ifdef __x86_64__
unsigned long long
test_u64_1 ()
{
  return _mm_crc32_u64 (0x9f65239602a24bf8, 0x894a58bff37dd4ba);
}

unsigned long long
test_u64_2 ()
{
  return _mm_crc32_u64 (0x06ef97970cd5e10f, 0x24334e2e832b42e7);
}

unsigned long long
test_u64_3 ()
{
  return _mm_crc32_u64 (0x5024a45450e739b8, 0x289ee1b7cefb382e);
}

unsigned long long
test_u64_4 ()
{
  return _mm_crc32_u64 (0xcbc89e1c3c2db116, 0xa89143dad3947f3d);
}

unsigned long long
test_u64_5 ()
{
  return _mm_crc32_u64 (-1, -1);
}
#endif

/* Test that there is no crc32 instruction emitted.  */
/* { dg-final { scan-assembler-not "crc32\[bwlq\]" } } */

/* Test the expected values.  */
/* { dg-final { scan-assembler-times "-1606234368," 1 } } */
/* { dg-final { scan-assembler-times "1776624948," 1 } } */
/* { dg-final { scan-assembler-times "-1269171002," 1 } } */
/* { dg-final { scan-assembler-times "-1190655916," 1 } } */
/* { dg-final { scan-assembler-times "16777215," 1 } } */

/* { dg-final { scan-assembler-times "350827643," 1 } } */
/* { dg-final { scan-assembler-times "1464882880," 1 } } */
/* { dg-final { scan-assembler-times "1604487598," 1 } } */
/* { dg-final { scan-assembler-times "-1181930003," 1 } } */
/* { dg-final { scan-assembler-times "65535," 1 } } */

/* { dg-final { scan-assembler-times "2122844148," 1 } } */
/* { dg-final { scan-assembler-times "881727033," 1 } } */
/* { dg-final { scan-assembler-times "139192539," 1 } } */
/* { dg-final { scan-assembler-times "1861871255," 1 } } */
/* { dg-final { scan-assembler-times "xorl\[\\t \]+%eax, %eax" 1 } } */

/* { dg-final { scan-assembler-times "3230864714" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "337502055" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "712052683" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "1390230251" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "3080238136" 1 { target { ! ia32 } } } } */
