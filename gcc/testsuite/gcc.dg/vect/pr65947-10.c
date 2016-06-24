/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#define N 32

/* Non-integer data types.  */

float
condition_reduction (float *a, float min_v)
{
  float last = 0;

  for (int i = 0; i < N; i++)
    if (a[i] < min_v)
      last = a[i];

  return last;
}

int
main (void)
{
  float a[N] = {
  11.5, 12.2, 13.22, 14.1, 15.2, 16.3, 17, 18.7, 19, 20,
  1, 2, 3.3, 4.3333, 5.5, 6.23, 7, 8.63, 9, 10.6,
  21, 22.12, 23.55, 24.76, 25, 26, 27.34, 28.765, 29, 30,
  31.111, 32.322
  };

  check_vect ();

  float ret = condition_reduction (a, 16.7);

  if (ret != (float)10.6)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" { xfail { ! vect_max_reduc } } } } */
/* { dg-final { scan-tree-dump-not "condition expression based on integer induction." "vect" } } */

