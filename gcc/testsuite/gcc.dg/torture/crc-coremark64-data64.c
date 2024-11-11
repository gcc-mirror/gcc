/* { dg-do run { target lp64 } } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */

#include <stdlib.h>

typedef unsigned long long int ee_u64;

__attribute__ ((noinline,optimize(0)))
ee_u64 crcu64_O0 (ee_u64 data, ee_u64 crc) {
  ee_u64 i = 0, x16 = 0, carry = 0;
  for (i = 0; i < 64; i++) {
      x16 = ((data & 1) ^ (crc & 1));
      data >>= 1;
      if (x16 == 1) {
	  crc ^= 0x4002123f4002123f;
	  carry = 1;
	} else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x8000000000000000;
      else
	crc &= 0x7fffffffffffffff;
    }
  return crc;
}

ee_u64 crcu64 (ee_u64 data, ee_u64 crc) {
  ee_u64 i = 0, x16 = 0, carry = 0;
  for (i = 0; i < 64; i++) {
      x16 = ((data & 1) ^ (crc & 1));
      data >>= 1;
      if (x16 == 1) {
	  crc ^= 0x4002123f4002123f;
	  carry = 1;
	} else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x8000000000000000;
      else
	crc &= 0x7fffffffffffffff;
    }
  return crc;
}

int main ()
{
  ee_u64 crc = 0;
  for (ee_u64 i = 0; i < 0xff; i++)
    {
      ee_u64 res1 = crcu64_O0 (i, crc);
      ee_u64 res2 = crcu64 (i, crc);
      if (res1 != res2)
	abort ();
      crc = res1;
    }
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 63" "crc" } } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
