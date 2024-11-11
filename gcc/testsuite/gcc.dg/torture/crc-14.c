/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

#include <stdlib.h>

typedef unsigned char uint8_t;

__attribute__ ((noinline,optimize(0)))
uint8_t gencrc_O0 (uint8_t *data)
{
  uint8_t crc = 0xff;
  size_t j;
  crc ^= *data;
  for (j = 0; j < 8; j++)
    {
      if ((crc & 0x80) != 0)
	crc = (uint8_t) ((crc << 1) ^ 0x31);
      else
	crc <<= 1;
    }
  return crc;
}

uint8_t gencrc (uint8_t *data)
{
  uint8_t crc = 0xff;
  size_t j;
  crc ^= *data;
  for (j = 0; j < 8; j++)
    {
      if ((crc & 0x80) != 0)
	crc = (uint8_t) ((crc << 1) ^ 0x31);
      else
	crc <<= 1;
    }
  return crc;
}

int main ()
{
  for (uint8_t i = 0; i < 255; i++)
    {
      uint8_t res1 = gencrc_O0 (&i);
      uint8_t res2 = gencrc (&i);
      if (res1 != res2)
	__builtin_abort ();
    }
  __builtin_exit (0);
}

/* { dg-final { scan-tree-dump "gencrc function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
