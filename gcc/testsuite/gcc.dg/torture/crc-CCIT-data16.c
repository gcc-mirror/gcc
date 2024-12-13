/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdlib.h>
#include <stdint.h>

__attribute__ ((noinline,optimize(0)))
uint16_t crc16_O0 (uint16_t data, uint16_t crc)
{
  int carry;
  for (int i = 0; i < 16; ++i) {
      carry = ((crc & 0x8000) ^ (data & 0x8000));
      crc <<= 1;
      data <<= 1;
      if (carry) crc ^= 0x1021;
    }
  return crc;
}

uint16_t crc16 (uint16_t data, uint16_t crc)
{
  int carry;
  for (int i = 0; i < 16; ++i) {
      carry = ((crc & 0x8000) ^ (data & 0x8000));
      crc <<= 1;
      data <<= 1;
      if (carry) crc ^= 0x1021;
    }
  return crc;
}

int main ()
{
  uint16_t crc = 0;

  for (uint16_t i = 0; i < 655; i++)
    {
      uint16_t res1 = crc16_O0 (i, crc);
      uint16_t res2 = crc16 (i, crc);
      crc = res1;
      if (res1 != res2)
	abort ();
      crc = res1;
    }
}

/* { dg-final { scan-tree-dump "crc16 function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 15" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
