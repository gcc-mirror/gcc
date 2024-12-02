/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

// Modified Coremark test, we don't detect.

#include <stdint.h>

uint16_t not_crcu8 (uint16_t data, uint16_t crc)
{
  uint16_t i = 0, carry = 0;
  for (i = 0; i < 16; i++)
    {
      if ((((crc & 1) == 1) && ((data & 1) == 0))
	  || (((crc & 1) == 0) && (data & 1) == 1))
	{
	  crc ^= 0x4002;
	  carry = 1;
	}
      else
	carry = 0;
      crc >>= 1;
      data >>= 1;
      if (carry)
	crc |= 0x8000;
      else
	crc &= 0x7fff;
    }
  return crc;
}
