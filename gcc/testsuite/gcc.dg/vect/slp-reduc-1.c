/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

unsigned int ub[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned int uc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

/* Vectorization of reduction using loop-aware SLP.  */

__attribute__ ((noinline))
int main1 (int n, int res0, int res1, int res2, int res3)
{
  int i;
  unsigned int udiff0 = 5, udiff1 = 10, udiff2 = 20, udiff3 = 30;

  for (i = 0; i < n; i++) {
    udiff3 += (ub[4*i + 3] - uc[4*i + 3]);
    udiff2 += (ub[4*i + 2] - uc[4*i + 2]);
    udiff1 += (ub[4*i + 1] - uc[4*i + 1]);
    udiff0 += (ub[4*i] - uc[4*i]);
  }

  /* Check results:  */
  if (udiff0 != res0
      || udiff1 != res1
      || udiff2 != res2
      || udiff3 != res3)
    abort ();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (N/4, 53, 66, 84, 102);
  main1 (N/4 - 1, 29, 40, 56, 72);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_add } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail vect_no_int_add } } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" } } */
