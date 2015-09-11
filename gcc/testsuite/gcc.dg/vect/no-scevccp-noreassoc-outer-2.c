/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40
int a[200*N+N];

__attribute__ ((noinline)) void
foo (){
  int i,j;
  int sum,s=0;

  for (i = 0; i < 200*N; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += (i + j);
      i++;
    }
    a[i] = sum;
  }
}

int main (void)
{
  int i,j,k=0;
  int sum,s=0;

  check_vect ();

  foo ();

    /* check results:  */
  for (i=0; i<200*N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++){
        sum += (j + i);
	i++;
      }
      if (a[i] != sum)
	abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail *-*-* } } } */
