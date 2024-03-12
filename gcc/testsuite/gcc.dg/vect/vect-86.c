/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int a[N];

__attribute__ ((noinline))
int main1 (int n)
{
  int i, j, k;
  int b[N];

  for (i = 0; i < n; i++)
    {
      for (j = 0; j < n; j++)
        {
	  k = i + n;
          a[j] = k;
        }
      b[i] = k;
    }

#pragma GCC novector
  for (j = 0; j < n; j++)
    if (a[j] != i + n - 1)
      abort();	

#pragma GCC novector
  for (i = 0; i < n; i++)
    if (b[i] != i + n)
      abort();	

  return 0;
}

int main (void)
{ 
  check_vect ();

  main1 (N);
  main1 (0);
  main1 (1);
  main1 (2);
  main1 (N-1);

  return 0;
}

/* Fails for targets that don't vectorize PLUS (e.g alpha).  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
