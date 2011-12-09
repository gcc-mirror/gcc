/* { dg-require-effective-target vect_int_mult } */

#include <stdarg.h>
#include "tree-vect.h"

#define K 4 

int in[2*K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int out[K];
int check_result[K] = {0,16,256,4096};

__attribute__ ((noinline)) void 
foo ()
{
  int sum;
  int i, j, k;

  for (k = 0; k < K; k++)
    {
      sum = 1;
      for (j = 0; j < K; j++) 
        for (i = 0; i < K; i++)
          sum *= in[i+k][j];
      out[k] = sum;
    }
}

int main ()
{
  int i, j, k;

  check_vect ();

  for (i = 0; i < 2*K; i++)
    for (j = 0; j < K; j++)
      in[i][j] = (i+2)/3;

  foo();

  for (k = 0; k < K; k++)
    if (out[k] != check_result[k])
      abort ();

  return 0;
}
        
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
      
