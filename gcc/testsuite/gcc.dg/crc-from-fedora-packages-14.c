/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - base64, slightly modified
// We don't support the case when leading bit of the polynomial is kept.

#include <stdint.h>
#include <stdlib.h>

uint32_t b64crc (const unsigned char* data, size_t ns)
{
  if (sizeof (unsigned int) < 4)
    exit (0);

  const unsigned char *s = data;
  uint32_t crc = 0xb704ceL;

  while (ns-- > 0)
    {
      int i;
      crc ^= (*s++) << 16;
      for (i = 0; i < 8; i++)
	{
	  crc <<= 1;
	  if (crc & 0x1000000)
	    crc ^= 0x1864cfbL;
	}
    }
  crc &= 0xffffff;
  /* ... */
  return crc;
}

/* { dg-final { scan-tree-dump "Polynomial's value is \\\{\[0, \]*0\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
