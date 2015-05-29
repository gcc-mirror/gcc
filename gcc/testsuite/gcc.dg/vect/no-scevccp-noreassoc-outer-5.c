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
    x = a[i];
    for (j = 0; j < N; j++) {
      sum += (x + j);
    }
    a[i] = sum + i + x;
  }
}

int main (void)
{
  int i,j;
  int sum;
  int aa[N];

  check_vect ();

  for (i=0; i<N; i++){
    a[i] = i;
    aa[i] = i;
  }
 
  foo ();

    /* check results:  */
  for (i=0; i<N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++)
        sum += (j + aa[i]);
      if (a[i] != sum + i + aa[i])
        abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" } } */
