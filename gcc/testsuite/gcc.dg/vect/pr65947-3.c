/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#define N 37

/* Non-simple condition reduction with additional variable and unsigned
   types.  */

unsigned int
condition_reduction (unsigned int *a, unsigned int min_v, unsigned int *b)
{
  unsigned int last = N + 65;
  unsigned int aval;

  for (unsigned int i = 0; i < N; i++)
    {
      aval = a[i];
      if (b[i] < min_v)
	last = aval;
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

  if (ret != 13)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "optimizing condition reduction with FOLD_EXTRACT_LAST" 2 "vect" { target vect_fold_extract_last } } } */
/* { dg-final { scan-tree-dump-not "condition expression based on integer induction." "vect" } } */
