/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm } */

#include "tree-vect.h"

#define ARR_SIZE 1024

void __attribute__((noipa))
foo (int a[][ARR_SIZE], int *b)
{
  int i;
  for (i = 0; i < ARR_SIZE; ++i)
    {
      a[0][i] += b[0];
      a[1][i] += b[1];
      a[2][i] += b[2];
      a[3][i] += b[3];
    }
}

int
main ()
{
  int a[4][ARR_SIZE];
  int b[4];

  check_vect ();

  for (int i = 0; i < 4; ++i)
    {
      b[i] = 20 * i;
      for (int j = 0; j < ARR_SIZE; ++j)
	a[i][j] = (i + 1) * ARR_SIZE - j;
    }

  foo (a, b);

  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < ARR_SIZE; ++j)
      if (a[i][j] != (i + 1) * ARR_SIZE - j + 20 * i)
	__builtin_abort ();

  return 0;

}

/* See that we do not try to vectorize the uniform CTORs.  */
/* { dg-final { scan-tree-dump-not "Analyzing vectorizable constructor" "slp1" } } */
