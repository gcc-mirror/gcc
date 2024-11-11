/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */
/* { dg-require-effective-target int32plus } */

/* This is a CRC, even though it may seem it's not.
   'j' is initialized with 4 (100) and increased by 2 each time.
   Thus, first bit value is never 1. For each iteration 'j & 1' yields 0,
   and it doesn't affect the CRC result.  */

#include <stdint.h>

uint16_t crc (uint8_t data, uint16_t crc) {
  uint8_t x16 = 0, carry = 0;
  for (uint8_t i = 0, j=4; i < 8; i++, j+=2) {
      x16 = (uint8_t) (((uint8_t) crc & 1)  ^ (j & 1));
      data >>= 1;
      if (x16 == 1) {
	  crc ^= 0x4002;
	  carry = 1;
	} else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x8000;
      else
	crc &= 0x7fff;
    }
  return crc;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
