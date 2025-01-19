/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdlib.h>
#include <stdint.h>

__attribute__ ((noinline))
uint16_t crc16_xor_outside (uint16_t data, uint16_t crc)
{
  int carry;
  crc ^= data;
  for (int i = 0; i < 16; ++i) {
      carry = ((crc & 0x8000));
      crc <<= 1;
      if (carry) crc ^= 0x1021;
    }
  return crc;
}

__attribute__ ((noinline))
uint16_t crc16_xor_inside (uint16_t data, uint16_t crc)
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
      uint16_t res1 = crc16_xor_outside (i, crc);
      uint16_t res2 = crc16_xor_inside (i, crc);
      crc = res1;
      if (res1 != res2)
	abort ();
    }

  if (crc16_xor_outside (0x1111, 0xFFFF) != 0x2f5d)
    abort ();

  if (crc16_xor_inside (0x1111, 0xFFFF) != 0x2f5d)
    abort ();
}

/* { dg-final { scan-tree-dump "crc16_xor_outside function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "crc16_xor_inside function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 15" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump-times "calculates CRC!" 2 "crc" } } */
