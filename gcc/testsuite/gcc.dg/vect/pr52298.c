/* { dg-additional-options "-O1 -fno-tree-pre -fno-tree-loop-im" } */

#include "tree-vect.h"

extern void abort (void);
int c[80];

__attribute__((noinline)) int
foo (void)
{
  int l = 0;
  int a, b;

  for (a = 3; a >= 0; a--)
    for (b = 7; b >= 0; b--)
      l |= c[a+60];
  return l;
}

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < 60; i++)
    c[i] = 1;
  for (; i < 64; i++)
    c[i] = 1 << (i - 59);
  if (foo () != 30)
    abort ();
  return 0;
}

