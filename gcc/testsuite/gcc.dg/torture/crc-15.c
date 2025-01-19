/* { dg-do compile } */
/* { dg-options "-w -fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

/* Test from busybox, we don't verify as it depends on endian variable.  */
#include <stdint.h>
#include <stdlib.h>

uint32_t* crc32_filltable(uint32_t *crc_table, int endian)
{
  uint32_t polynomial = endian ? 0x04c11db7 : 0xedb88320;
  uint32_t c;
  unsigned i, j;

  if (!crc_table)
    crc_table = malloc (256 * sizeof (uint32_t));

  for (i = 0; i < 256; i++)
    {
      c = endian ? (i << 24) : i;
      for (j = 8; j; j--)
	{
	  if (endian)
	    c = (c & 0x80000000) ? ((c << 1) ^ polynomial) : (c << 1);
	  else
	    c = (c & 1) ? ((c >> 1) ^ polynomial) : (c >> 1);
	}
      *crc_table++ = c;
    }

  return crc_table - 256;
}
