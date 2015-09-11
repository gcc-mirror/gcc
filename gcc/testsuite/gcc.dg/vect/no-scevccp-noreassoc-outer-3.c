/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

int a[N];

__attribute__ ((noinline)) int
foo (){
  int i,j;
  int sum,x;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += (i + j);
    }
    a[i] = sum;
  }
}

int main (void)
{
  int i,j;
  int sum;

  check_vect ();

  foo ();

    /* check results:  */
  for (i=0; i<N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++){
        sum += (j + i);
      }
      if (a[i] != sum)
        abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" } } */
