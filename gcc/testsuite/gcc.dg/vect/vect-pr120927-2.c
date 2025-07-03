/* { dg-additional-options "--param vect-partial-vector-usage=1" } */
/* { dg-additional-options "-mavx512bw -mavx512vl" { target avx512f_runtime } } */

#include "tree-vect.h"

static const double __attribute__((aligned(__BIGGEST_ALIGNMENT__))) a[] = { 1., 2., 3., 4., 5. };

void __attribute__((noipa))
foo (double *b, double *bp, double c, int n)
{
  for (int i = 0; i < n; ++i)
    b[i] = bp[i] = a[i] * c;
}

int main()
{
  double b[6], bp[6];
  b[5] = bp[5] = 13.;
  check_vect ();
  foo (b, bp, 3., 5);
  if (b[5] != 13. || bp[5] != 13.)
    abort ();
  return 0;
}
