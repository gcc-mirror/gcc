/* PR tree-optimization/63189 */
/* { dg-additional-options "-fwrapv" } */

#include "tree-vect.h"

__attribute__((noinline, noclone)) static int
bar (const short *a, int len)
{
  int x;
  int x1 = 0;

  for (x = 0; x < len; x++)
    x1 += x * a[x];
  return x1;
}

__attribute__((noinline, noclone)) void
foo (void)
{
  short stuff[9] = {1, 1, 1, 1, 1, 1, 1, 1, 1 };
  if (bar (stuff, 9) != 36)
    abort ();
}

int
main ()
{
  check_vect ();
  foo ();
  return 0;
}

