/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

unsigned __attribute__((noipa))
bar (unsigned int *x)
{
  int sum = 4;
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  for (int i = 0; i < 16; ++i)
    sum += x[i];
  return sum;
}

int
main()
{
  static int a[16] __attribute__((aligned(__BIGGEST_ALIGNMENT__)))
    = { 1, 3, 5, 8, 9, 10, 17, 18, 23, 29, 30, 55, 42, 2, 3, 1 };
  check_vect ();
  if (bar (a) != 260)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
