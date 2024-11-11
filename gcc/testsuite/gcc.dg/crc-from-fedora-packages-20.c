/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details  -O2" } */
/* { dg-require-effective-target int32plus } */

// File - scorelayout.i
// It is verified by the pass if we don't filter out the cases when data's size
// and loop iteration count differ.
#include <stdlib.h>
typedef unsigned char guchar;
typedef unsigned int guint32;
guint32
bit_reverse (guint32 x)
{
  x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
  x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
  x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
  x = (x << 24) | ((x & 0xFF00) << 8) | ((x >> 8) & 0xFF00) | (x >> 24);
  return x;
}

guint32
crc32 (guchar * message)
{
  if (sizeof (int) < 4)
    exit (0);

  int i, j;
  guint32 byte, crc;
  i = 0;
  crc = 0xFFFFFFFF;
  while (message[i] != 0)
    {
      byte = message[i];
      byte = bit_reverse (byte);
      for (j = 0; j <= 7; j++)
	{
	  if ((int) (crc ^ byte) < 0)
	    crc = (crc << 1) ^ 0x04C11DB7;
	  else
	    crc = crc << 1;
	  byte = byte << 1;
	}
      i = i + 1;
    }
  return bit_reverse (~crc);
}

/* { dg-final { scan-tree-dump "Loop iteration number and data's size differ." "crc" } } */
