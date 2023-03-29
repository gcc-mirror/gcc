/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#define N 32

/* Simple condition reduction where the result is a negative of the induction.
   Will fail to vectorize to a simple case.  */

signed int
condition_reduction (signed int *a, signed int min_v)
{
  signed int last = -1;

  for (signed int i = 0; i < N; i++)
    if (a[i] < min_v)
      last = -i;

  return last;
}

int
main (void)
{
  signed int a[N] = {
  11, -12, 13, 14, 15, 16, 17, 18, 19, 20,
  1, 2, -3, 4, 5, 6, 7, -8, 9, 10,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32
  };

  check_vect ();

  signed int ret = condition_reduction (a, 16);

  if (ret != -19)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "optimizing condition reduction with FOLD_EXTRACT_LAST" 2 "vect" { target { vect_fold_extract_last && { ! vect_multiple_sizes } } } } } */
/* { dg-final { scan-tree-dump "optimizing condition reduction with FOLD_EXTRACT_LAST" "vect" { target { vect_fold_extract_last && vect_multiple_sizes } } } } */
/* { dg-final { scan-tree-dump-not "condition expression based on integer induction." "vect" } } */
