/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#define N 255

/* Condition reduction with maximum possible loop size.  Will fail to
   vectorize because the vectorisation requires a slot for default values.  */

signed char __attribute__((noinline,noclone))
condition_reduction (signed char *a, signed char min_v)
{
  signed char last = -72;

  for (int i = 0; i < N; i++)
    if (a[i] < min_v)
      last = a[i];

  return last;
}

int
main ()
{
  signed char a[N] = {
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32
  };
  __builtin_memset (a+32, 43, N-32);

  check_vect ();

  signed char ret = condition_reduction (a, 16);
  if (ret != 10)
    abort ();

  ret = condition_reduction (a, 1);
  if (ret != -72)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" { target { ! vect_fold_extract_last } } } } */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" { target vect_fold_extract_last } } } */
/* { dg-final { scan-tree-dump "loop size is greater than data size" "vect" { target { ! vect_fold_extract_last } } } } */
/* { dg-final { scan-tree-dump-times "optimizing condition reduction with FOLD_EXTRACT_LAST" 2 "vect" { target vect_fold_extract_last } } } */
/* { dg-final { scan-tree-dump-not "condition expression based on integer induction." "vect" } } */
