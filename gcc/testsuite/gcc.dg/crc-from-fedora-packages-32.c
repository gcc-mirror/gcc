/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - reliable_text.c
#include <stdint.h>
char calculateCRC8_(char* input, int length)
{
  unsigned char generator = 0x1D;
  unsigned char crc = 0x00;

  while (length > 0)
    {
      unsigned char ch = *input++;
      length--;


      if (ch == 0) break;

      crc ^= ch;

      for (int i = 0; i < 8; i++)
	{
	  if ((crc & 0x80) != 0)
	    {
	      crc = (unsigned char)((crc << 1) ^ generator);
	    }
	  else
	    {
	      crc <<= 1;
	    }
	}
    }

  return crc;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
