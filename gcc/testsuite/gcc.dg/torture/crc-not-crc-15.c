/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-O3" "-flto" } } */

/* With -O3 the cycles is split into 2,
   and one of them calculates CRC (when data != 0).
   Thus, when compiling with -O3 there is CRC. */

#include <stdint.h>

uint8_t not_crc(uint8_t crc, uint8_t data) {
  crc = crc ^ data;

  for (uint8_t i = 0, n = 0; i < 8; i++, n++) {
      if ((crc & 0x01) && data)
	crc = (crc >> 1) ^ 0x8C;
      else
	crc >>= 1;
    }
  return crc;
}

/* { dg-final { scan-tree-dump-times "calculates CRC!" 0 "crc" } } */
