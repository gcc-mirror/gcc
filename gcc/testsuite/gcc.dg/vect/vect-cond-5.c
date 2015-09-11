/* { dg-require-effective-target vect_condition } */

#include <stdarg.h>
#include "tree-vect.h"

#define K 32

int cond_array[2*K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int a[K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int out[K];
int check_result[K] = {2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

__attribute__ ((noinline)) void 
foo (int c)
{
  int res, i, j, k, next;

  for (k = 0; k < K; k++)
    {
      res = 0;
      for (j = 0; j < K; j++) 
        for (i = 0; i < K; i++)
          { 
            next = a[i][j]; 
            res = c > cond_array[i+k][j] ? next : res;
          }
 
      out[k] = res;
    }
}

int main ()
{
  int i, j, k;

  check_vect ();

  for  (j = 0; j < K; j++)
    {
      for (i = 0; i < 2*K; i++)
        cond_array[i][j] = i+j;

      for (i = 0; i < K; i++)
        a[i][j] = i+2;
    }

  foo(5);

  for (k = 0; k < K; k++)
    if (out[k] != check_result[k])
      abort ();

  return 0;
}

/* Double reduction with cond_expr is not supported, since even though the order 
   of computation is the same, but vector results should be reduced to scalar
   result, which can'be done for cond_expr.  */
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail *-*-* } } } */
      
