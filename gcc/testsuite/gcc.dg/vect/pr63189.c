/* PR tree-optimization/63189 */

#include "tree-vect.h"

short int d[16] = { 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 };

__attribute__((noinline, noclone)) void
foo (void)
{
  int j, s = 0;
  for (j = 0; j < 8; j++)
    s += d[j] * j;
  if (s != 7)
    abort ();
}

int
main ()
{
  check_vect ();
  foo ();
  return 0;
}

