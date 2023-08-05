/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

float b[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
float a[N];
float c[N];

__attribute__ ((noinline))
int main1 (int n)
{
  int i=0;

  /* Vectorized: unknown loop bound.  */
  while (n--) {
    a[i] = b[i];
    i++;
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

__attribute__ ((noinline))
int main2 (unsigned int n)
{
  int i=0;
  int nn = n;

  /* Vectorized: unknown loop bound.  */
  while (n--) {
    c[i] = b[i];
    i++;
  }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < nn; i++)
    {
      if (c[i] != b[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  main1 (N);
  main2 (N);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
