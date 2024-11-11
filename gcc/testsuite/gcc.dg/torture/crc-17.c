/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

#include <stdlib.h>

__attribute__ ((noinline,optimize(0)))
unsigned short calc_crc_O0 (unsigned short crc, unsigned char data)
{
 unsigned int i, j, org, dst;
 org = data;
 dst = 0;

 for (i = 0; i < 8; i++) {
  org <<= 1;
  dst >>= 1;
  if (org & 0x100)
   dst |= 0x80;
 }
 data = (unsigned char) dst;
 crc ^= (unsigned int) data << (16 - 8);
 for (j = 0; j < 8; j++) {
  if (crc & 0x8000U)
   crc = (crc << 1) ^ 0x1021U ;
  else
   crc <<= 1 ;
 }
 return crc;
}

unsigned short calc_crc (unsigned short crc, unsigned char data)
{
  unsigned int i, j, org, dst;
  org = data;
  dst = 0;

  for (i = 0; i < 8; i++) {
      org <<= 1;
      dst >>= 1;
      if (org & 0x100)
	dst |= 0x80;
    }
  data = (unsigned char) dst;
  crc ^= (unsigned int) data << (16 - 8);
  for (j = 0; j < 8; j++) {
      if (crc & 0x8000U)
	crc = (crc << 1) ^ 0x1021U ;
      else
	crc <<= 1 ;
    }
  return crc;
}


int main ()
{
  unsigned short crc = 0x0D80;
  for (unsigned char i = 0; i < 255; i++)
    {
      unsigned short res1 = calc_crc_O0 (crc, i);
      unsigned short res2 = calc_crc (crc, i);
      if (res1 != res2)
	__builtin_abort ();
      crc = res1;
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "calc_crc function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump-times "Polynomial's value is \\\{\[0, \]*1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1\\\}" 1 "crc"} } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
