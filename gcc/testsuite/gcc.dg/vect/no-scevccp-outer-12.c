/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

int a[N];
short b[N];

__attribute__ ((noinline)) int
foo (){
  int i,j;
  int sum;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += j;
    }
    a[i] = sum;
    b[i] = (short)sum;
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
      for (j = 0; j < N; j++)
        sum += j;
      if (a[i] != sum  || b[i] != (short)sum)
        abort();
    }

  return 0;
}

/* Until we support multiple types in the inner loop  */
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail { ! aarch64*-*-* } } } } */
