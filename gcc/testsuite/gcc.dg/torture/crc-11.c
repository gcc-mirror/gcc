/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

//We just don't check cases when the polynomial is a variable, as we can't replace it.
#include <stdint.h>

uint16_t crc16(uint16_t crc, uint8_t a, uint16_t polynom) {
  int i;
  crc ^= a;
  for (i = 0; i < 8; ++i) {
      if (crc & 1)
	crc = (crc >> 1) ^ polynom;
      else
	crc = (crc >> 1);
    }
  return crc;
}

/* { dg-final { scan-tree-dump "Second operand of the xor statement isn't an integer constant.\n" "crc" } } */
