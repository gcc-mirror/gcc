/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - cold-flash.i
#include <stdint.h>
uint32_t tab[256];

void crc32_gentab(void) {

  int i, j;
  uint32_t crc;
  uint32_t poly = 0xEDB88320L;

  for ( i = 0; i < 256; i++) {

      crc = i;

      for ( j = 8; j > 0; j-- ) {

	  if ( crc & 1 )
	    crc = (crc >> 1) ^ poly;
	  else
	    crc >>= 1;

	}

      tab[i] = crc;

    }

}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
