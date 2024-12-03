/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -w" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-O3" "-flto" } } */

#include <stdlib.h>
#define CRC32_POLY_BE 0x04c11db7
#define RETVAL_OUT_OF_MEMORY		(-6)
#define RETVAL_NOT_BZIP_DATA		(-2)
#define RETVAL_OK			0

struct bunzip_data {
    unsigned int crc32Table[256];
};


int start_bunzip(struct bunzip_data **bdp, void *inbuf, long len,
			     long (*fill)(void*, unsigned long))
{
  if (sizeof (unsigned int) <= 3)
    exit (0);

  struct bunzip_data *bd;
  unsigned int i, j, c;

  /* Figure out how much data to allocate */
  i = sizeof(struct bunzip_data);

  /* Allocate bunzip_data.  Most fields initialize to zero. */
  bd = *bdp = malloc(i);

  /* ... */

  /* Init the CRC32 table (big endian) */
  for (i = 0; i < 256; i++) {
      c = i << 24;
      for (j = 8; j; j--)
	c = c&0x80000000 ? (c << 1)^(CRC32_POLY_BE) : (c << 1);
      bd->crc32Table[i] = c;
    }

  /* . . . */
  return RETVAL_OK;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
