/* { dg-do compile { target lp64 } } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - CRC-2.i
#include <stdint.h>
unsigned long long CRCTable[256];
void XS_Digest__CRC__crc64() {
  unsigned long long poly64rev = 0xd800000000000000ULL;
  unsigned long long part;
  int i, j;
  for (i = 0; i < 256; i++) {
      part = i;
      for (j = 0; j < 8; j++) {
	  if (part & 1)
	    part = (part >> 1) ^ poly64rev;
	  else
	    part >>= 1;
	}
      CRCTable[i] = part;
    }
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
