/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - wifi.  Sizeof check is added for the test.

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
typedef unsigned int u32;
typedef unsigned char u8;
u32 WIFI_CRC32Table[256];
bool initialized;
u32 reflect (u32 ref, char ch)
{
  if (sizeof (u32) < 4)
    exit (0);

  u32 value = 0;

  for (int i = 1; i < (ch + 1); i++)
    {
      if (ref & 1)
	value |= 1 << (ch - i);
      ref >>= 1;
    }

  return value;
}

u32 WIFI_calcCRC32 (u8 *data, int len)
{
  if (sizeof (u32) < 4)
    exit (0);

  u32 crc = 0xFFFFFFFF;

  while (len--)
    crc = (crc >> 8) ^ WIFI_CRC32Table[(crc & 0xFF) ^ *data++];

  return (crc ^ 0xFFFFFFFF);
}

void WIFI_initCRC32Table ()
{
  if (sizeof (u32) < 4)
    exit (0);

  initialized = false;
  if (initialized) return;
  initialized = true;

  u32 polynomial = 0x04C11DB7;

  for (int i = 0; i < 0x100; i++)
    {
      WIFI_CRC32Table[i] = reflect (i, 8) << 24;
      for (int j = 0; j < 8; j++)
	WIFI_CRC32Table[i] = (WIFI_CRC32Table[i] << 1)
			     ^ (WIFI_CRC32Table[i] & (1 << 31) ? polynomial
							       : 0);
      WIFI_CRC32Table[i] = reflect (WIFI_CRC32Table[i], 32);
    }
}

int main ()
{
  if (sizeof (u32) < 4)
    exit (0);

  WIFI_initCRC32Table ();
  WIFI_calcCRC32 ("dfsf", 4);
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
