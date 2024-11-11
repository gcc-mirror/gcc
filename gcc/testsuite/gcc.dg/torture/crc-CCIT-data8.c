/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdint.h>
#include <stdlib.h>

__attribute__ ((noinline,optimize(0)))
uint16_t crc16_O0 (uint8_t data, uint16_t crc) {
  int carry;
  for (int i = 0; i < 8; ++i) {
      carry = ((crc & 0x8000) >> 8 ^ (data & 0x80));
      crc <<= 1;
      data <<= 1;
      if (carry) crc ^= 0x1021;
    }
  return crc;
}

uint16_t crc16 (uint8_t data, uint16_t crc) {
  int carry;
  for (int i = 0; i < 8; ++i) {
      carry = ((crc & 0x8000) >> 8 ^ (data & 0x80));
      crc <<= 1;
      data <<= 1;
      if (carry) crc ^= 0x1021;
    }
  return crc;
}

int main ()
{
  uint16_t crc = 0x0D80;
  for (uint8_t i = 0; i < 255; i++)
    {
      uint16_t res1 = crc16_O0 (i, crc);
      uint16_t res2 = crc16 (i, crc);
      if (res1 != res2)
	abort ();
      crc = res1;
    }
}

/* { dg-final { scan-tree-dump "crc16 function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
