/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

__attribute__ ((noinline)) int
foo (){
  int i,j;
  int sum,s=0;

  for (i = 0; i < 200*N; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += (i + j);
      i++;
    }
    s += sum;
  }
  return s;
}

__attribute__ ((noinline))
int bar (int i, int j)
{
return (i + j);
}

int main (void)
{
  int i,j,k=0;
  int sum,s=0;
  int res; 

  check_vect ();

  res = foo ();

    /* check results:  */
  for (i=0; i<200*N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++){
        sum += bar (i, j);
	i++;
      }
      s += sum;
    }
  if (res != s)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
