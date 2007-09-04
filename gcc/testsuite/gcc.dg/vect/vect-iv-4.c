/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
 
__attribute__ ((noinline)) int main1 ()
{  
  unsigned int arr1[N];
  unsigned short arr2[N];
  unsigned int k = 0;
  unsigned short m = 3;
  int i = 0;
  
  /* Vectorization of induction with multiple data types.  */

   do { 
        k = k + 2;
        arr1[i] = k;
	m = k + 3;
	arr2[i] = m;
	i++;
   } while (i < N);

  /* check results:  */
  for (i = 0; i < N; i++)
    { 
      if (arr1[i] != 2+2*i || arr2[i] != 5 + 2*i)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_pack_trunc } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
