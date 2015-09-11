/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

int a[N];

__attribute__ ((noinline)) int
foo (){
  int i,j;
  int sum;

  for (i = 0; i < N/2; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += j;
    }
    a[2*i] = sum;
    a[2*i+1] = 2*sum;
  }
}

int main (void)
{
  int i,j;
  int sum;

  check_vect ();

  for (i=0; i<N; i++)
    a[i] = i;
 
  foo ();

    /* check results:  */
  for (i=0; i<N/2; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++)
        sum += j;
      if (a[2*i] != sum || a[2*i+1] != 2*sum)
        abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { target { vect_interleave || vect_strided2 } } } } */
