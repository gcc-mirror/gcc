/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - hash.c.i.
/* We don't support cases with multiple conditional branches in the loop.  */
#include <stdint.h>
typedef struct _crc_calculator {
    uint32_t remainder;
    uint32_t trunc_poly;
    uint32_t bits;
    uint32_t table[256];
    uint32_t final_result_mask;
} CRC_CALCULATOR;

void crc_calculator_init_table(CRC_CALCULATOR *p_crc_calculator) {
  const uint32_t high_bit = 1 << (p_crc_calculator->bits - 1);
  const uint32_t byte_high_bit = 1 << (8 - 1);

  for (uint32_t value = 0; value < 256; value++) {
      uint32_t remainder = 0;
      for (uint8_t mask = byte_high_bit; mask != 0; mask >>= 1) {
	  if (value & mask) {
	      remainder ^= high_bit;
	    }

	  if (remainder & high_bit) {
	      remainder <<= 1;
	      remainder ^= p_crc_calculator->trunc_poly;
	    } else {
	      remainder <<= 1;
	    }
	}
      p_crc_calculator->table[value] = remainder;
    }
}
