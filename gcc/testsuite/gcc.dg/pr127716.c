/* PR tree-optimization/127716 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fcompare-debug -fdump-tree-crc-details" } */

typedef unsigned short u16;

#define POLY 0x1021

u16
crc16 (u16 data)
{
  int i = 0;
  u16 sh;

  do
    {
      sh = data << 1;
      if (data & 0x8000)
	data = sh ^ POLY;
      else
	data = sh;
    }
  while (++i < 16);

  u16 last __attribute__ ((unused)) = sh;
  return data;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump "# DEBUG last => NULL" "crc" } } */
