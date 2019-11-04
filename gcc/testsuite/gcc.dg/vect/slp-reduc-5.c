/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

int c[N];

/* Vectorization of reduction using loop-aware SLP.  */

__attribute__ ((noinline))
int main1 (int n, int res0, int res1)
{
  int i;
  int max0 = -100, max1 = -313;

  for (i = 0; i < n; i++) {
    max1 = max1 < c[2*i+1] ? c[2*i+1] : max1;
    max0 = max0 < c[2*i] ? c[2*i] : max0;
  }

  /* Check results:  */
  if (max0 != res0
      || max1 != res1)
    abort ();

  return 0;
}

int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    c[i] = (i+3) * -1;

  c[0] = c[1] = -100;
  main1 (N/2, -5, -6);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { xfail vect_no_int_min_max } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail vect_no_int_min_max } } } */

