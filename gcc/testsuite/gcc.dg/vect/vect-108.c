/* { dg-require-effective-target vect_int_mult } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
int ia[N];

__attribute__ ((noinline)) int
main1 (void)
{
  int i;

  /* This loop is vectorized on platforms that support vect_int_mult.  */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] * ic[i];
    }

  /* Check results.  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] * ic[i])
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
