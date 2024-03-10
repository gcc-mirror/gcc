/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

float b[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
float a[N];

__attribute__ ((noinline))
int main1 (int n)
{
  int i;

  /* Vectorized: unknown loop bound).  */
  for (i = 0; i < n; i++){
    a[i] = b[i];
  }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < n; i++)
    {
      if (a[i] != b[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 (N);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
