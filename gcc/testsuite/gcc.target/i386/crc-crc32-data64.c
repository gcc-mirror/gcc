/* { dg-do run { target { sse4_runtime && pclmul_runtime } } } */
/* { dg-options "-O2 -msse4.2 -mpclmul -masm=att -save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-flto"} } */

#include <stdint.h>
#include <stdlib.h>

#define POLY 0x1EDC6F4100000000ULL

uint64_t expand8to64 (uint64_t x) {
  x ^= x >> 30;
  x *= UINT64_C(0xbf58476d1ce4e5b9);
  x ^= x >> 27;
  x *= UINT64_C(0x94d049bb133111eb);
  x ^= x >> 31;
  return x;
}

__attribute__ ((noinline,optimize(0)))
uint64_t _crc32_u64_O0 (uint64_t crc, uint64_t data) {
  const uint64_t POLY_REV = __builtin_bitreverse64(POLY);
  int i;
  crc = crc ^ data;

  for (i = 0; i < 64; i++) {
      if (crc & 1)
	crc = (crc >> 1) ^ POLY_REV;
      else
	crc = (crc >> 1);
    }

  return crc;
}

uint64_t _crc32_u64 (uint64_t crc, uint64_t data) {
  return __builtin_rev_crc64_data64(crc, data, POLY);
}

int main ()
{
  uint64_t crc = 0x0D800D80;
  for (uint32_t i = 0; i <= 0xff; i++)
    {
      uint64_t data = expand8to64(i);
      uint64_t res1 = _crc32_u64_O0 (crc, data);
      uint64_t res2 = _crc32_u64 (crc, data);
      if (res1 != res2)
	abort ();
      crc = res1;
    }
}

/* { dg-final { scan-assembler "crc32q" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "crc32l" { target ia32 } } } */
/* { dg-final { scan-assembler-not "pclmul"} } */
