/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - png_tools.i
#include <stdint.h>

uint32_t gen_grAb_crc(unsigned char *buf)
{
  uint32_t crc = 0xffffffff;
  uint32_t crc_table[256];
  uint32_t c;
  int32_t n, k;
  for (n = 0; n < 256; n++) {
      c = (uint32_t) n;
      for (k = 0; k < 8; k++) {
	  if (c & 1)
	    c = ((uint32_t) 0xedb88320) ^ (c >> 1);
	  else
	    c = c >> 1;
	}
      crc_table[n] = c;
    }
  for (n = 0; n < 12; n++)
    crc = crc_table[(crc ^ buf[n]) & 0xff] ^ (crc >> 8);
  return crc ^ ((uint32_t) 0xffffffff);
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
