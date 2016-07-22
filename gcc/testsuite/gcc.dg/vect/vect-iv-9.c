/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 26
int a[N];
 
__attribute__ ((noinline)) int main1 (int X)
{  
  int s = X;
  int i;

  /* vectorization of reduction with induction.  */
  for (i = 0; i < N; i++)
    s += (i + a[i]);

  return s;
}

int main (void)
{ 
  int s, i;
  check_vect ();
  
  for (i = 0; i < N; i++)
    a[i] = 2*i;

  s = main1 (3);
  if (s != 978)
    abort ();

  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
