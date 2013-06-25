/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
int result[N] = {8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38};
 
__attribute__ ((noinline, noclone)) int main1 (int X)
{  
  int arr[N];
  int k = 3;
  int m, i=0;
  
   /* Vectorization of induction with non-constant step X.  */

   do { 
        m = k + 5;
        arr[i] = m;
        k = k + X;
	i++;
   } while (i < N);

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (arr[i] != result[i])
	abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 (2);
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
