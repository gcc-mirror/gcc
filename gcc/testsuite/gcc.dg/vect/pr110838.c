#include "tree-vect.h"

short a[32], b[32];

void __attribute__((noipa)) foo ()
{
  for (int i = 0; i < 32; ++i)
    a[i] = b[i] >> 16;
}

void __attribute__((noipa)) bar (int n)
{
  int np = n & 31;
  for (int i = 0; i < 32; ++i)
    a[i] = b[i] >> np;
}

int main ()
{
  check_vect ();
  b[0] = -8;
  foo ();
  if (a[0] != -1)
    abort ();
  bar (16);
  if (a[0] != -1)
    abort ();
  return 0;
}
