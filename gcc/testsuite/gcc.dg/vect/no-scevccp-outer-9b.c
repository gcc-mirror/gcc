/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

int a[N];

__attribute__ ((noinline)) int
foo (int n){
  int i,j;
  int sum;

  if (n<=0)
    return 0;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < n; j++) {
      sum += j;
    }
    a[i] = sum;
  }
}

int main (void)
{
  int i,j;
  int sum;

  check_vect ();

  for (i=0; i<N; i++)
    a[i] = i;
 
  foo (N);

    /* check results:  */
  for (i=0; i<N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++)
        sum += j;
      if (a[i] != sum)
        abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" } } */
