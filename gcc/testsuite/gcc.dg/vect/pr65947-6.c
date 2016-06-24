/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#define N 30

/* Condition reduction where loop type is different than the data type.  */

int
condition_reduction (int *a, int min_v)
{
  int last = N + 65;

  for (char i = 0; i < N; i++)
    if (a[i] < min_v)
      last = a[i];

  return last;
}


int
main (void)
{
  int a[N] = {
  67, 32, 45, 43, 21, -11, 12, 3, 4, 5,
  6, 76, -32, 56, -32, -1, 4, 5, 6, 99,
  43, 22, -3, 22, 16, 34, 55, 31, 87, 324
  };

  check_vect ();

  int ret = condition_reduction (a, 16);

  if (ret != -3)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" { xfail { ! vect_max_reduc } } } } */
/* { dg-final { scan-tree-dump-not "condition expression based on integer induction." "vect" } } */
