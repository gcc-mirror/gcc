/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - crc.i.
// We don't support this case.

#include <stdlib.h>
#include <stdint.h>
uint32_t crc24_calculate(uint32_t preset, const uint8_t *data, uint8_t len)
{
  uint32_t state = preset;
  uint8_t i;

  for (i = 0; i < len; i++) {
      uint8_t n, cur = data[i];

      for (n = 0; n < 8; n++) {
	  int next_bit = (state ^ cur) & 1;

	  cur >>= 1;
	  state >>= 1;
	  if (next_bit) {
	      state |= 1 << 23;
	      state ^= 0x5a6000;
	    }
	}
    }

  return state;
}

uint32_t crc24_reverse(uint32_t crc, const uint8_t *data, uint8_t len)
{
  uint32_t state = crc;
  uint8_t i;

  for (i = 0; i < len; i++) {
      uint8_t n, cur = data[len - i - 1];

      for (n = 0; n < 8; n++) {
	  int top_bit = state >> 23;

	  state = (state << 1) & 0xffffff;
	  state |= top_bit ^ ((cur >> (7 - n)) & 1);
	  if (top_bit)
	    state ^= 0xb4c000;
	}
    }

  return state;
}
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
