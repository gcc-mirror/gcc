/* PR tree-optimization/56918 */
/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

extern void abort (void);
double data[8];

__attribute__((noinline, noclone)) void
foo ()
{
  int i;
  for (i = 0; i < 8; ++i)
    data[i] = ((i + 2) % 3) + 1;
}

int
main ()
{
  int i;
  check_vect ();
  foo ();
  if (data[0] != 3 || data[7] != 1)
    abort ();
#pragma GCC novector
  for (i = 1; i < 4; ++i)
    if (data[i] != i || data[i + 3] != i)
      abort ();
  return 0;
}

