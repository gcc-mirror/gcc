/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#define N 37

/* Re-use the result of the condition inside the loop.  Will fail to
   vectorize.  */

unsigned int
condition_reduction (unsigned int *a, unsigned int min_v, unsigned int *b)
{
  unsigned int last = N + 65;

  for (unsigned int i = 0; i < N; i++)
    {
      if (b[i] < min_v)
	last = i;
      a[i] = last;
    }
  return last;
}

int
main (void)
{
  unsigned int a[N] = {
  31, 32, 33, 34, 35, 36, 37,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20
  };
  unsigned int b[N] = {
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  31, 32, 33, 34, 35, 36, 37
  };

  check_vect ();

  unsigned int ret = condition_reduction (a, 16, b);

  if (ret != 29)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
