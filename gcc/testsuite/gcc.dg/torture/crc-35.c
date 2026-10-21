/* { dg-do run { target lp64 } } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdint.h>
#include <stdlib.h>

// CRC64_ECMA_182
#define CRC64_POLY 0x42F0E1EBA9EA3693

// Test bit-forward CRC64 16-bit data
__attribute__ ((noinline,optimize(0)))
uint64_t _crc64_O0 (uint16_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 16; i++) {
      if (((crc & 0x8000000000000000) >> 48 ^ (data & 0x8000)))
	crc = (crc << 1) ^ CRC64_POLY;
      else
	crc = (crc << 1);
      data <<= 1;
    }
  return crc;
}

uint64_t _crc64 (uint16_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 16; i++) {
      if (((crc & 0x8000000000000000) >> 48 ^ (data & 0x8000)))
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

  for (uint16_t i = 0; i <= 0xff; i++)
    {
      uint16_t data = (i << 8) | (~i); // cover 2-byte range
      uint64_t res1 = _crc64_O0 (data, crc);
      uint64_t res2 = _crc64 (data, crc);
      crc = res2;
      if (res1 != res2)
	abort ();
    }
}

/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 15" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
