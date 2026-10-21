#include "tree-vect.h"

long o[2];

__attribute__ ((noipa)) void
f (long a, long b, int c)
{
  __label__ lab;
  __attribute__ ((noinline)) void nest (int d)
  {
    if (d == c)
      goto lab;
  }
  long x = a, y = b;
  nest (0);
  x = a + 1;
  y = b + 2;
  nest (1);
  x = a + 3;
  y = b + 4;
  nest (2);
  o[0] = 0;
  o[1] = 0;
  return;
lab:
  o[0] = x;
  o[1] = y;
}

int
main ()
{
  check_vect ();

  f (10, 20, 0);
  if (o[0] != 10 || o[1] != 20)
    __builtin_abort ();
  f (10, 20, 1);
  if (o[0] != 11 || o[1] != 22)
    __builtin_abort ();
  f (10, 20, 2);
  if (o[0] != 13 || o[1] != 24)
    __builtin_abort ();
  f (10, 20, 3);
  if (o[0] != 0 || o[1] != 0)
    __builtin_abort ();
  return 0;
}
