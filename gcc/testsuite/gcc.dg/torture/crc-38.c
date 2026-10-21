/* { dg-do run { target lp64 } } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdint.h>
#include <stdlib.h>

#define CRC64_POLY1 0x9A6C9329AC4BC9B5
#define CRC64_POLY2 0x1A6C9329AC4BC9B4

// Test bit-reverse CRC64 8-bit data with Poly having LSB=1
__attribute__ ((noinline,optimize(0)))
uint64_t _crc64_poly1_O0 (uint8_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 8; i++) {
      if (((crc & 0x1) ^ (data & 0x1)))
	crc = (crc >> 1) ^ CRC64_POLY1;
      else
	crc = (crc >> 1);
      data >>= 1;
    }
  return crc;
}

uint64_t _crc64_poly1 (uint8_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 8; i++) {
      if (((crc & 0x1) ^ (data & 0x1)))
	crc = (crc >> 1) ^ CRC64_POLY1;
      else
	crc = (crc >> 1);
      data >>= 1;
    }
  return crc;
}

// Test bit-reverse CRC64 8-bit data with Poly having LSB=0
__attribute__ ((noinline,optimize(0)))
uint64_t _crc64_poly2_O0 (uint8_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 8; i++) {
      if (((crc & 0x1) ^ (data & 0x1)))
	crc = (crc >> 1) ^ CRC64_POLY2;
      else
	crc = (crc >> 1);
      data >>= 1;
    }
  return crc;
}

uint64_t _crc64_poly2 (uint8_t data, uint64_t crc) {
  int i;
  for (i = 0; i < 8; i++) {
      if (((crc & 0x1) ^ (data & 0x1)))
	crc = (crc >> 1) ^ CRC64_POLY2;
      else
	crc = (crc >> 1);
      data >>= 1;
    }
  return crc;
}

int main ()
{
  uint64_t crc1 = 0;
  uint64_t crc2 = 0;

  for (uint16_t i = 0; i <= 0xff; i++)
    {
      uint8_t data = (uint8_t)i;
      uint64_t res1 = _crc64_poly1_O0 (data, crc1);
      uint64_t res2 = _crc64_poly1 (data, crc1);
      crc1 = res2;
      if (res1 != res2)
	abort ();

      uint64_t res3 = _crc64_poly2_O0 (data, crc2);
      uint64_t res4 = _crc64_poly2 (data, crc2);
      crc2 = res4;
      if (res3 != res4)
	abort ();
    }
}

/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit reverse" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
