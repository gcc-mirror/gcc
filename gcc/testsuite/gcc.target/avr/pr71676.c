/* { dg-do run } */
/* { dg-options "-Os -fno-tree-switch-conversion" } */

#include "exit-abort.h"

volatile unsigned char y;

__attribute__((noinline,noclone))
unsigned char foo (unsigned long x) 
{
  switch (x)
    {
    case 0:	y = 67; break;
    case 1:	y = 20; break;
    case 2:	y = 109; break;
    case 3:	y = 33; break;
    case 4:	y = 44; break;
    case 5:	y = 37; break;
    case 6:	y = 10; break;
    case 7:	y = 98; break;
    }
  return y;
}

int main (void)
{
  if (0 != foo (7L + 0x10000L))
    abort();
  return 0;
}
