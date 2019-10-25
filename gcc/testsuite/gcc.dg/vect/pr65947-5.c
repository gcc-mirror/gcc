/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

extern void abort (void) __attribute__ ((noreturn));

#if VECTOR_BITS > 256
#define N (VECTOR_BITS / 8)
#else
#define N 32
#endif

/* Condition reduction where loop size is not known at compile time.  Will fail
   to vectorize.  Version inlined into main loop will vectorize.  */

unsigned char
condition_reduction (unsigned char *a, unsigned char min_v, int count)
{
  unsigned char last = 65;

  for (int i = 0; i < count; i++)
    if (a[i] < min_v)
      last = a[i];

  return last;
}

int
main (void)
{
  unsigned char a[N] = {
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32
  };
  for (int i = 32; i < N; ++i)
    {
      a[i] = 70 + (i & 3);
      asm volatile ("" ::: "memory");
    }

  check_vect ();

  unsigned char ret = condition_reduction (a, 16, N);

  if (ret != 10)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" { target { ! vect_fold_extract_last } } } } */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" { target vect_fold_extract_last } } } */
/* { dg-final { scan-tree-dump "loop size is greater than data size" "vect" { xfail vect_fold_extract_last } } } */
/* { dg-final { scan-tree-dump-times "optimizing condition reduction with FOLD_EXTRACT_LAST" 2 "vect" { target vect_fold_extract_last } } } */
/* { dg-final { scan-tree-dump-not "condition expression based on integer induction." "vect" } } */
