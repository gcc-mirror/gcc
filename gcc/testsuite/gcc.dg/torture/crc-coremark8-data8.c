/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdlib.h>

typedef unsigned char ee_u8;

__attribute__ ((noinline,optimize(0)))
ee_u8 crcu8_O0(ee_u8 data, ee_u8 crc) {
  ee_u8 i = 0, x16 = 0, carry = 0;
  for (i = 0; i < 8; i++) {
      x16 = ((data & 1) ^ (crc & 1));
      data >>= 1;
      if (x16 == 1) {
	  crc ^= 0x40;
	  carry = 1;
	} else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x80;
      else
	crc &= 0x7f;
    }
  return crc;
}

ee_u8 crcu8(ee_u8 data, ee_u8 crc) {
  ee_u8 i = 0, x16 = 0, carry = 0;
  for (i = 0; i < 8; i++) {
      x16 = ((data & 1) ^ (crc & 1));
      data >>= 1;
      if (x16 == 1) {
	  crc ^= 0x40;
	  carry = 1;
	} else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x80;
      else
	crc &= 0x7f;
    }
  return crc;
}

int main ()
{
  ee_u8 crc = 0;
  for (ee_u8 i = 0; i < 0xff; i++)
    {
      ee_u8 res1 = crcu8_O0 (i, crc);
      ee_u8 res2 = crcu8 (i, crc);
      if (res1 != res2)
	abort ();
      crc = res1;
    }
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
