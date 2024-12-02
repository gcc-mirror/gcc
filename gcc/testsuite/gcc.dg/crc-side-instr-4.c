/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

#include <stdint.h>

uint16_t crc16_update(uint16_t crc, uint8_t a) {
  int i;
  uint16_t b;
  for (i = 0; i < 8; ++i) {
      if ((crc & 1) ^ (a & 1))
	crc = (crc >> 1) ^ 0xa001;
      else
	crc = (crc >> 1);
      a >>= 1;
      b = crc;
    }
  uint16_t c = b++; // This is removed from the compiled code.
  return crc;
}

/* { dg-final { scan-tree-dump "crc16_update function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1\\\}" "crc" } } */
