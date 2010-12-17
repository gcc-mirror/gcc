/* { dg-require-effective-target vect_condition } */

#include <stdlib.h> 
#include "tree-vect.h"

#define N 16

int c[N] = {3,2,1,10,1,42,3,4,50,9,32,8,11,10,1,2};
int a[N+1] = {0,16,32,48,64,128,256,512,0,16,32,48,64,128,256,512,1024};

__attribute__ ((noinline)) void 
foo (int *x)
{
  int i;
  int curr_a, flag, next_a;

  curr_a = a[0];

  for (i = 0; i < N; i++) 
    {
      flag = *x > c[i];
      next_a = a[i+1];
      curr_a = flag ? curr_a : next_a;
    }

  *x = curr_a;
}

int main (void)
{
  int x = 7;

  check_vect ();

  foo (&x);

  if (x != 256)
    abort ();

  return 0;
}

/* The order of computation should not be changed for cond_expr, therefore, 
   it cannot be vectorized in reduction.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */


