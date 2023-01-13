/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

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

/* { dg-final { scan-tree-dump "crc24_calculate function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 32" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reverse" "crc"} } */