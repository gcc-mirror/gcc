/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - crc32-2.i

#include <stdint.h>
#include <stddef.h>
void *xmalloc(size_t size) __attribute__ ((malloc));
uint32_t* crc32_filltable(uint32_t *crc_table, int endian)
{
  uint32_t polynomial = endian ? 0x04c11db7 : 0xedb88320;
  uint32_t c;
  unsigned i, j;

  if (!crc_table)
    crc_table = xmalloc(256 * sizeof(uint32_t));

  for (i = 0; i < 256; i++) {
      c = endian ? (i << 24) : i;
      for (j = 8; j; j--) {
	  if (endian)
	    c = (c&0x80000000) ? ((c << 1) ^ polynomial) : (c << 1);
	  else
	    c = (c&1) ? ((c >> 1) ^ polynomial) : (c >> 1);
	}
      *crc_table++ = c;
    }

  return crc_table - 256;
}
