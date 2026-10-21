/* { dg-do run { target lp64 } } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdint.h>
#include <stdlib.h>

// CRC64_ECMA_182
#define CRC64_POLY 0x42F0E1EBA9EA3693

uint32_t expand8to32 (uint32_t x) {
  x ^= x >> 16;
  x *= 0x7feb352dU;
  x ^= x >> 15;
  x *= 0x846ca68bU;
  x ^= x >> 16;
  return x;
}

// Test bit-forward CRC64 32-bit data
__attribute__ ((noinline,optimize(0)))
uint64_t _crc64_O0 (uint32_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 32; i++) {
      if (((crc & 0x8000000000000000) >> 32 ^ (data & 0x80000000)))
	crc = (crc << 1) ^ CRC64_POLY;
      else
	crc = (crc << 1);
      data <<= 1;
    }
  return crc;
}

uint64_t _crc64 (uint32_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 32; i++) {
      if (((crc & 0x8000000000000000) >> 32 ^ (data & 0x80000000)))
	crc = (crc << 1) ^ CRC64_POLY;
      else
	crc = (crc << 1);
      data <<= 1;
    }
  return crc;
}

int main ()
{
  uint64_t crc = 0;

  for (uint32_t i = 0; i <= 0xff; i++)
    {
      uint32_t data = expand8to32 (i);
      uint64_t res1 = _crc64_O0 (data, crc);
      uint64_t res2 = _crc64 (data, crc);
      crc = res2;
      if (res1 != res2)
	abort ();
    }
}

/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 31" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
