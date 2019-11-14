/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

unsigned int ub[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned int uc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

/* Vectorization of reduction using loop-aware SLP (with unrolling).  */

__attribute__ ((noinline))
int main1 (int n, int res0, int res1, int res2, int res3)
{
  int i;
  unsigned int udiff0 = 5, udiff1 = 10;

  for (i = 0; i < n; i++) {
    udiff1 += (ub[2*i + 1] - uc[2*i + 1]);
    udiff0 += (ub[2*i] - uc[2*i]);
  }

  /* Check results:  */
  if (udiff0 != res0
      || udiff1 != res1)
    abort ();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (N/2, 117, 138, 84, 102);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_add } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail vect_no_int_add } } } */

