/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

// Test from roms/ipxe/src/util/zbin.c
// We don't detect this case, as second operand of the xor is a variable.

#include <stdint.h>
#include <stddef.h>

#define CRCPOLY 0xedb88320
#define CRCSEED 0xffffffff

uint32_t crc32_le ( uint32_t crc, const void *data, size_t len ) {
  const uint8_t *src = data;
  uint32_t mult;
  unsigned int i;

  while ( len-- ) {
      crc ^= *(src++);
      for ( i = 0 ; i < 8 ; i++ ) {
	  mult = ( ( crc & 1 ) ? CRCPOLY : 0 );
	  crc = ( ( crc >> 1 ) ^ mult );
	}
    }
  return crc;
}
