/* PR tree-optimization/63189 */
/* { dg-do run } */

#include "tree-vect.h"

static const short a[8] = {1, 1, 1, 1, 1, 1, 1, 1 };
static const unsigned char b[8] = {0, 0, 0, 0, 0, 0, 0, 0 };

__attribute__((noinline, noclone)) static int
bar (void)
{
  int sum = 0, i;
  for (i = 0; i < 8; ++i)
    sum += a[i] * b[i];
  return sum;
}

__attribute__((noinline, noclone)) void
foo (void)
{
  if (bar () != 0)
    abort ();
}

int
main ()
{
  check_vect ();
  foo ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
