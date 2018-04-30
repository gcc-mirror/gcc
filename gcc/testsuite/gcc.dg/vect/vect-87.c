/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-peeling-for-alignment=0" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

__attribute__ ((noinline))
int main1 (int n, int *a)
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


  for (j = 0; j < n; j++)
    if (a[j] != i + n - 1)
      abort();	

  for (j = 0; j < n; j++)
    if (b[j] != j + n)
      abort();	

  return 0;
}

int main (void)
{ 
  int a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

  check_vect ();

  main1 (N, a);
  main1 (0, a);
  main1 (1, a);
  main1 (2, a);
  main1 (N-1, a);

  return 0;
}

/* Fails for targets that don't vectorize PLUS (e.g alpha).  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" {target { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } */
