/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */
/* { dg-require-effective-target int32plus } */

#include <stdlib.h>

__attribute__ ((noinline,optimize(0)))
unsigned short crc16_O0 (unsigned char newByte, unsigned short crcValue) {
  unsigned char i;

  for (i = 0; i < 8; i++) {

      if (((crcValue & 0x8000) >> 8) ^ (newByte & 0x80)) {
	  crcValue = (crcValue << 1) ^ 0x102;
	} else {
	  crcValue = (crcValue << 1);
	}

      newByte <<= 1;
    }

  return crcValue;
}

unsigned short crc16 (unsigned char newByte, unsigned short crcValue) {
  unsigned char i;

  for (i = 0; i < 8; i++) {

      if (((crcValue & 0x8000) >> 8) ^ (newByte & 0x80)) {
	  crcValue = (crcValue << 1) ^ 0x102;
	} else {
	  crcValue = (crcValue << 1);
	}

      newByte <<= 1;
    }

  return crcValue;
}

int main ()
{
  unsigned short crc = 0x0D80;
  for (unsigned char i = 0; i < 255; i++)
    {
      unsigned short res1 = crc16_O0 (crc, i);
      unsigned short res2 = crc16 (crc, i);
      if (res1 != res2)
	__builtin_abort ();
      crc = res1;
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
