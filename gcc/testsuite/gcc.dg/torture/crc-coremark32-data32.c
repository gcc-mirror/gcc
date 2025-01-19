/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */
/* { dg-require-effective-target int32plus } */

#include <stdlib.h>

typedef unsigned int ee_u32;

__attribute__ ((noinline,optimize(0)))
ee_u32 crcu32_O0 (ee_u32 data, ee_u32 crc) {
  ee_u32 i = 0, x16 = 0, carry = 0;
  for (i = 0; i < 32; i++) {
      x16 = ((data & 1) ^ (crc & 1));
      data >>= 1;
      if (x16 == 1) {
	  crc ^= 0x4002123;
	  carry = 1;
	} else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x80000000;
      else
	crc &= 0x7fffffff;
    }
  return crc;
}

ee_u32 crcu32 (ee_u32 data, ee_u32 crc) {
  ee_u32 i = 0, x16 = 0, carry = 0;
  for (i = 0; i < 32; i++) {
      x16 = ((data & 1) ^ (crc & 1));
      data >>= 1;
      if (x16 == 1) {
	  crc ^= 0x4002123;
	  carry = 1;
	} else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x80000000;
      else
	crc &= 0x7fffffff;
    }
  return crc;
}

int main ()
{
  ee_u32 crc = 0;
  for (ee_u32 i = 0; i < 0xff; i++)
    {
      ee_u32 res1 = crcu32_O0 (i, crc);
      ee_u32 res2 = crcu32 (i, crc);
      if (res1 != res2)
	abort ();
      crc = res1;
    }
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 31" "crc" } } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
