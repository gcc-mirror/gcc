/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

unsigned int ub[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,
    0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned int uc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
    0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

/* Vectorization of reduction using loop-aware SLP (with two copies).  */

__attribute__ ((noinline))
int main1 (int n, int res0, int res1, int res2, int res3,
	   int res4, int res5, int res6, int res7)
{
  int i;
  unsigned int udiff0 = 5, udiff1 = 10, udiff2 = 20, udiff3 = 30;
  unsigned int udiff4 = 45, udiff5 = 50, udiff6 = 60, udiff7 = 70;

  for (i = 0; i < n; i++) {
    udiff7 += (ub[8*i + 7] - uc[8*i + 7]);
    udiff6 += (ub[8*i + 6] - uc[8*i + 6]);
    udiff5 += (ub[8*i + 5] - uc[8*i + 5]);
    udiff4 += (ub[8*i + 4] - uc[8*i + 4]);
    udiff3 += (ub[8*i + 3] - uc[8*i + 3]);
    udiff2 += (ub[8*i + 2] - uc[8*i + 2]);
    udiff1 += (ub[8*i + 1] - uc[8*i + 1]);
    udiff0 += (ub[8*i] - uc[8*i]);
  }

  /* Check results:  */
  if (udiff0 != res0
      || udiff1 != res1
      || udiff2 != res2
      || udiff3 != res3
      || udiff4 != res4
      || udiff5 != res5
      || udiff6 != res6
      || udiff7 != res7)
    abort ();

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (N/8, 37, 50, 68, 86, 109, 122, 140, 158);
  main1 (N/8 - 1, 21, 32, 48, 64, 85, 96, 112, 128);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_add } } } */
/* For variable-length SVE, the number of scalar statements in the
   reduction exceeds the number of elements in a 128-bit granule.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail { vect_no_int_add || { aarch64_sve && vect_variable_length } } } } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" { xfail { aarch64_sve && vect_variable_length } } } } */
