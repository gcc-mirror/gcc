/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-O3" "-flto" } } */

 #include <stdint.h>

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

/* { dg-final { scan-tree-dump "crc24_reverse function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
