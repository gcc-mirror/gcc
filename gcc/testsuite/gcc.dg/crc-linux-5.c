/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

#include <stddef.h>
#include <stdint.h>
/* We don't find, as we can't replace the first loop,
   and in the second loop polynomials leading 1 bit is kept too
   (second function's iteration count is 4).  */
typedef unsigned char  u8;
typedef unsigned short u16;
u8 drm_dp_msg_data_crc4(const uint8_t *data, u8 number_of_bytes)
{
  u8 bitmask = 0x80;
  u8 bitshift = 7;
  u8 array_index = 0;
  int number_of_bits = number_of_bytes * 8;
  u16 remainder = 0;

  while (number_of_bits != 0) {
      number_of_bits--;
      remainder <<= 1;
      remainder |= (data[array_index] & bitmask) >> bitshift;
      bitmask >>= 1;
      bitshift--;
      if (bitmask == 0) {
	  bitmask = 0x80;
	  bitshift = 7;
	  array_index++;
	}
      if ((remainder & 0x100) == 0x100)
	remainder ^= 0xd5;
    }

  number_of_bits = 8;
  while (number_of_bits != 0) {
      number_of_bits--;
      remainder <<= 1;
      if ((remainder & 0x100) != 0)
	remainder ^= 0xd5;
    }

  return remainder & 0xff;
}

/* sideband msg handling */
u8 drm_dp_msg_header_crc4(const uint8_t *data, size_t num_nibbles)
{
  u8 bitmask = 0x80;
  u8 bitshift = 7;
  u8 array_index = 0;
  int number_of_bits = num_nibbles * 4;
  u8 remainder = 0;

  while (number_of_bits != 0) {
      number_of_bits--;
      remainder <<= 1;
      remainder |= (data[array_index] & bitmask) >> bitshift;
      bitmask >>= 1;
      bitshift--;
      if (bitmask == 0) {
	  bitmask = 0x80;
	  bitshift = 7;
	  array_index++;
	}
      if ((remainder & 0x10) == 0x10)
	remainder ^= 0x13;
    }

  number_of_bits = 4;
  while (number_of_bits != 0) {
      number_of_bits--;
      remainder <<= 1;
      if ((remainder & 0x10) != 0)
	remainder ^= 0x13;
    }

  return remainder;
}

/* { dg-final { scan-tree-dump "drm_dp_msg_data_crc4 function maybe contains CRC calculation." "crc" } } */
