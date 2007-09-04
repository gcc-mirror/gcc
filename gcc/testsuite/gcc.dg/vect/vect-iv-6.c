/* { dg-require-effective-target vect_int } */
#include <stdio.h>
#include <stdarg.h>
#include "tree-vect.h"

#define N 16
 
__attribute__ ((noinline)) int main1 (int X)
{  
  int arr1[N+1];
  int arr2[N+1];
  int k = X;
  int m, i=0;
  
   /* Vectorization of induction with non-constant initial condition X. 
      Also we have here two uses of the induction-variable k as defined
      by the loop-header phi (as opposed to the other uses of k that are
      defined in the loop), in which case we exercise the fact that we
      reuse the same vector def-use-cycle for both uses. 
      Peeling to align the store is also applied. This peeling also aligns
      the load (as they have the same misalignment).  */

   do { 
	arr2[i+1] = 2*k;
        k = k + 2;
        arr1[i+1] = k;
        k = k + 4;
	i++;
   } while (i < N);

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (arr1[i+1] != X+6*i+2
	  || arr2[i+1] != 2*(X+6*i))
	abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 (3);
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
