/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define K 32

int in[2*K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int out[K];
int check_result[K] = {63,63,191,191,127,127,191,191,127,127,191,191,127,127,191,191,127,127,191,191,127,127,191,191,127,127,191,191,127,127,191,191};

__attribute__ ((noinline)) void 
foo ()
{
  int res_or, res_and, res_xor, i, j, k;

  for (k = 0; k < K; k++)
    {
      res_or = 0;
      for (j = 0; j < K; j++) 
        for (i = 0; i < K; i++) 
          res_or = res_or | in[i+k][j];
 
      res_and = 1;
      for (j = 0; j < K; j++)
        for (i = 0; i < K; i++)
          res_and = res_and & in[i+k][j];

      res_xor = 0;
      for (j = 0; j < K; j++)
        for (i = 0; i < K; i++)
          res_xor = res_xor ^ in[i+k][j];

      out[k] = res_or + res_and + res_xor;
    }
}

int main ()
{
  int i, j, k;

  check_vect ();

  for  (j = 0; j < K; j++)
    {
      for (i = 0; i < 2*K; i++)
        in[i][j] = i+j;

      for (i = 0; i < K; i++)
        out[i] = i+j;
    }

  foo();

  for (k = 0; k < K; k++)
    if (out[k] != check_result[k])
      abort ();

  return 0;
}
        
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 3 "vect" } } */
      
