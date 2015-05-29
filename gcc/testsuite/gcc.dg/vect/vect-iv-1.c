/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
int result[N] = {8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38};
 
__attribute__ ((noinline)) int main1 (int X)
{  
  int arr[N];
  int k = X;
  int m, i=0;
  
   /* vectorization of induction.  */

   do { 
        m = k + 5;
        arr[i] = m;
        k = k + 2;
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
  
  return main1 (3);
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
