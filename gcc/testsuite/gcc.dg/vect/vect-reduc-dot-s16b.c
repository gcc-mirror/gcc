/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

#define DOT 43680

signed short X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
signed short Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* short->short->int dot product.  Should be vectorized on architectures
   supporting vectorized multiplication of two short args with short result,
   e.g "mulv4hi3" and widenning sum */
__attribute__ ((noinline)) int
foo (int len)
{
  int i;
  int result = 0;
  short prod;

  for (i = 0; i < len; i++)
    {
      prod = X[i] * Y[i];
      result += prod;
    }
  return result;
}

int
main (void)
{
  int i, dot;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      X[i] = i;
      Y[i] = 64 - i;
      __asm__ volatile ("");
    }

  dot = foo (N);
  if (dot != DOT)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_short_mult && { vect_widen_sum_hi_to_si || vect_unpack } } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { ! vect_short_mult } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { { ! vect_widen_sum_hi_to_si } && { ! vect_unpack } } } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */

