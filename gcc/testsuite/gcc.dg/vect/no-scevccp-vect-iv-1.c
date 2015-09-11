/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 26
 
__attribute__ ((noinline)) int main1 (int X)
{  
  int s = X;
  int i;

  /* vectorization of reduction with induction. 
     Need -fno-tree-scev-cprop or else the loop is eliminated.  */
  for (i = 0; i < N; i++)
    s += i;

  return s;
}

int main (void)
{ 
  int s;
  check_vect ();
  
  s = main1 (3);
  if (s != 328)
    abort ();

  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
