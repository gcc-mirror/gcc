/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int __attribute__((noipa))
foo (int n, int* p, int* pi)
{
  int sum = 0;
  for (int i = 0; i != n; i++)
    {
      if (pi[i] > 0)
        sum -= p[i];
    }
  return sum;
}

int p[16] __attribute__((aligned(__BIGGEST_ALIGNMENT__)))
  = { 7, 3, 1, 4, 9, 10, 14, 7, -10, -55, 20, 9, 1, 2, 0, -17 };
int pi[16] __attribute__((aligned(__BIGGEST_ALIGNMENT__)))
  = { 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1 };
int
main()
{
  check_vect ();

  if (foo (16, p, pi) != 57)
    abort ();
  return 0;
}
