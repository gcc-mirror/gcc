/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
 
__attribute__ ((noinline)) int main1 ()
{  
  float arr[N];
  float f = 1.0;
  int i;
  
  /* Vectorization of fp induction.  */

  for (i=0; i<N; i++)
    {
      arr[i] = f;
      f += 2.0;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (arr[i] != 1.0 + 2.0*i)
	abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail {! arm_neon_ok } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
