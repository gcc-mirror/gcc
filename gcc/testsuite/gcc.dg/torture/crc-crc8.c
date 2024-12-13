/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdint.h>
#include <stdlib.h>

__attribute__ ((noinline,optimize(0)))
uint8_t crc8_O0 (uint8_t data, uint8_t crc)
{
  int carry;
  for (int i = 0; i < 8; ++i) {
      carry = ((crc & 0x80) ^ (data & 0x80));
      crc <<= 1;
      data <<= 1;
      if (carry) crc ^= 0x21;
    }
  return crc;
}

uint8_t crc8 (uint8_t data, uint8_t crc)
{
  int carry;
  for (int i = 0; i < 8; ++i) {
      carry = ((crc & 0x80) ^ (data & 0x80));
      crc <<= 1;
      data <<= 1;
      if (carry) crc ^= 0x21;
    }
  return crc;
}

int main ()
{
  uint8_t crc = 0x0D;
  for (uint8_t i = 0; i < 255; i++)
    {
      uint8_t res1 = crc8_O0 (i, crc);
      uint8_t res2 = crc8 (i, crc);
      if (res1 != res2)
	abort ();
      crc = res1;
    }
}

/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
