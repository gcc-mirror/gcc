/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

#include <stdlib.h>

__attribute__ ((noinline,optimize(0)))
unsigned char crc8_O0 (unsigned char value)
{
    for (int i = 0; i < 8; ++i) {
        value = (value & 0x80) ? ((value << 1) ^ 0x31) : (value << 1);
    }

    return value;
}

unsigned char crc8 (unsigned char value)
{
  for (int i = 0; i < 8; ++i) {
      value = (value & 0x80) ? ((value << 1) ^ 0x31) : (value << 1);
    }

  return value;
}

int main ()
{
  for (unsigned char i = 0; i < 255; i++)
    {
      unsigned char res1 = crc8_O0 (i);
      unsigned char res2 = crc8 (i);
      if (res1 != res2)
	__builtin_abort ();
    }
  __builtin_exit (0);
}

/* { dg-final { scan-tree-dump "crc8 function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{\[0, \]*0, 0, 1, 1, 0, 0, 0, 1\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
