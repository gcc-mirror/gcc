/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#define N 27

/* Condition reduction with multiple types in the comparison.  Will fail to
   vectorize.  */

int
condition_reduction (char *a, int min_v)
{
  int last = N + 65;

  for (int i = 0; i < N; i++)
    if (a[i] < min_v)
      last = a[i];

  return last;
}


int
main (void)
{
  char a[N] = {
  1, 28, 3, 48, 5, 68, 7, -88, 89, 180,
  121, 122, -123, 124, 12, -12, 12, 67, 84, 122,
  67, 55, 112, 22, 45, 23, 111
  };

  check_vect ();

  int ret = condition_reduction (a, 16);

  if (ret != 12)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "multiple types in double reduction or condition reduction" "vect" { xfail { ! vect_max_reduc } } } } */
