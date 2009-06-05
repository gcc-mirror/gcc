/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40


__attribute__ ((noinline)) int
foo (int *a){
  int i,j;
  int sum;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += j;
    }
    a[i] = sum;
  }
}

int main (void)
{
  int i,j;
  int sum;
  int a[N];

  check_vect ();

  for (i=0; i<N; i++)
    a[i] = i;
 
  foo (a);

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

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail { ! { vect_hw_misalign } } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
