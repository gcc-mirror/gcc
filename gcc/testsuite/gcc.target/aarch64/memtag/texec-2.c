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

int main (void)
{
  volatile unsigned char *ptr = __builtin_alloca (15);

  setHandler();

  use (ptr);
  ptr[0x10] = 0x55;
  return 0;
}
