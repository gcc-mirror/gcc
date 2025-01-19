/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc -O2" } */

#include <stdint.h>

uint8_t not_crc(uint8_t crc, uint8_t data) {
  crc = crc ^ data;

  for (uint8_t i = 0, n = 0; i < 8; i++, n++) {
      if (data > i)
	crc = 8;
      if (crc & 0x01)
	crc = (crc >> 1) ^ 0x8C;
      else
	crc >>= 1;
    }
  return crc;
}

/* { dg-final { scan-tree-dump-times "calculates CRC!" 0 "crc" } } */
