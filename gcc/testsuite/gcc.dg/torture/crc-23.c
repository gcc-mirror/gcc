/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details -ftree-cselim" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-flto" } } */
/* { dg-require-effective-target int32plus } */

// Modified example from crc-from-fedora-packages-24.c

#include <stdint.h>
#include <stdlib.h>

__attribute__ ((noinline,optimize(0)))
void crc_byte_O0 (const char data, uint16_t *crc16)
{
  int k;
  uint16_t c,d ;

  c = data << 8 ;
  d = c;

  for (k = 0; k < 16; k++) {
      *crc16 = (c & 0x8000) ^ *crc16;

      if (*crc16 & 0x8000) {
	  *crc16 = *crc16 << 1;
	  *crc16 = *crc16 ^ 0x8005;
	} else
	*crc16 = *crc16 << 1;

      d = d << 1;
      c = d;
    }
}

void crc_byte (const char data, uint16_t *crc16)
{
  int k;
  uint16_t c,d ;

  c = data << 8 ;
  d = c;

  for (k = 0; k < 16; k++) {
      *crc16 = (c & 0x8000) ^ *crc16;

      if (*crc16 & 0x8000) {
	  *crc16 = *crc16 << 1;
	  *crc16 = *crc16 ^ 0x8005;
	} else
	*crc16 = *crc16 << 1;

      d = d << 1;
      c = d;
    }
}

int main ()
{
  uint16_t crc = 0x0D80;
  for (char i = 0; i < 127; i++)
    {
      uint16_t res1 = crc, res2 = crc;
      crc_byte_O0 (i, &res1);
      crc_byte (i, &res2);

      if (res1 != res2)
	__builtin_abort ();
      crc = res1;
    }
  __builtin_exit (0);
}

/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 15" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
