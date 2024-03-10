/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

__attribute__ ((noinline))
int main1 (int *a)
{
  int i, j, k;
  int b[N];

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
	  k = i + N;
          a[j] = k;
        }
      b[i] = k;
    }


#pragma GCC novector
  for (j = 0; j < N; j++)
    if (a[j] != i + N - 1)
      abort();	

#pragma GCC novector
  for (j = 0; j < N; j++)
    if (b[j] != j + N)
      abort();	

  return 0;
}

int main (void)
{ 
  int a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

  check_vect ();

  main1 (a);

  return 0;
}

/* Fails for targets that don't vectorize PLUS (e.g alpha).  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
