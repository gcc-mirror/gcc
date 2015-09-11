/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

int a[N];

/* induction variable k advances through inner and outer loops.  */

__attribute__ ((noinline)) int
foo (int n){
  int i,j,k=0;
  int sum;

  if (n<=0)
    return 0;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < n; j+=2) {
      sum += k++;
    }
    a[i] = sum + j;
  }
}

int main (void)
{
  int i,j,k=0;
  int sum;

  check_vect ();

  for (i=0; i<N; i++)
    a[i] = i;
 
  foo (N);

    /* check results:  */
  for (i=0; i<N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j+=2)
        sum += k++;
      if (a[i] != sum + j)
        abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail *-*-* } } } */
