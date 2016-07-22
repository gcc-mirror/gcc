/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
 
__attribute__ ((noinline))
int main1 ()
{  
  int arr1[N];
  int k = 0;
  int m = 3, i = 0;
  
  /* Vectorization of induction that is used after the loop.  */

   do { 
        k = k + 2;
        arr1[i] = k;
	m = m + k;
	i++;
   } while (i < N);

  /* check results:  */
  for (i = 0; i < N; i++)
    { 
      if (arr1[i] != 2+2*i)
        abort ();
    }

  return m + k;
}

int main (void)
{ 
  int res;

  check_vect ();
  
  res = main1 ();
  if (res != 32 + 275)
    abort ();

  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
