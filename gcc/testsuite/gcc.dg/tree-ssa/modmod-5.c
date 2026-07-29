/* { dg-do run } */
/* { dg-options "-O2 -ftrapv" } */

#include <limits.h>

__attribute__ ((noipa))
static int
f (int x)
{
  return (x % 12) % -1;
}

int
main (void)
{
  if (f (INT_MIN) != 0 || f (-1) != 0 || f (0) != 0 || f (INT_MAX) != 0)
    __builtin_abort ();
  return 0;
}
