/* { dg-do run { xfail *-*-* } } */
/* { dg-require-effective-target memtag_exec } */
/* { dg-additional-options "-O2" } */

#include "mte-sig.h"

void  __attribute__((noinline))
use (volatile unsigned char *ptr)
{
  ptr[0] = 0x41;
  ptr[1] = 0x42;
}

int main(void)
{
  volatile unsigned char array[15];
  volatile unsigned char *ptr = &array[0];

  setHandler();
  use (ptr);

  /* Write to memory beyond the 16 byte granule (offsest 0x10) MTE should
     generate an exception If the offset is less than 0x10 no SIGSEGV will
     occur.  */
  ptr[0x10] = 0x55;
  return 0;
}
