/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - mkiss.i
// We don't verify as CRC's value is known.
#include <stdint.h>
typedef unsigned short int u16;

u16 crctab[256];
void init_crc(void)
{
  short int i, j;
  u16 accum, data;

  for (i = 0; i < 256; i++) {
      accum = 0xffff;
      data = i;
      for (j = 0; j < 8; ++j) {
	  if ((data^accum) & 0x0001)


	    accum = (accum >> 1) ^ 0x8408;
	  else

	    accum >>= 1;
	  data >>= 1;
	}
      crctab[i] = accum;
    }
}
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
