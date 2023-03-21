/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details -march=rv64gc_zbc" } */
#include <stdio.h>
unsigned short crc16(unsigned char newByte, unsigned short crcValue) {
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
  unsigned short crc = 0;

  crc = crc16(0x12, crc);
  printf("%04X\n", crc);  // 1224

  crc = crc16(0x34, crc);
  printf("%04X\n", crc);  // 024C

  crc = crc16(0x35, crc);
  printf("%04X\n", crc);  // 7B6E

}

/* { dg-final { scan-tree-dump "crc16 function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 16" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "crc16 function calculates CRC!" "crc"} } */
