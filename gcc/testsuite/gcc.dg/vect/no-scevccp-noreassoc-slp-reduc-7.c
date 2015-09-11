/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define MAX 121

unsigned int ub[N] = {0,3,6,9,12,15,18,121,24,27,113,33,36,39,42,45};

/* Vectorization of reduction using loop-aware SLP (with unrolling).  */

__attribute__ ((noinline))
int main1 (int n)
{
  int i;
  unsigned int max = 50;

  for (i = 0; i < n; i++) {
    max = max < ub[2*i] ? ub[2*i] : max;
    max = max < ub[2*i + 1] ? ub[2*i + 1] : max;
  }

  /* Check results:  */
  if (max != MAX)
    abort ();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (N/2);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_min_max } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail vect_no_int_min_max } } } */

