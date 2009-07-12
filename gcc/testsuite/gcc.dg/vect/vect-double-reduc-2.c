/* { dg-require-effective-target vect_int_mult } */

#include <stdarg.h>
#include <stdio.h>
#include "tree-vect.h"

#define K 32

int in[2*K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int coeff[K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int out[K];
int check_result[K] = {357184,339264,321344,303424,285504,267584,249664,231744,213824,195904,177984,160064,142144,124224,106304,88384,70464,52544,34624,16704,-1216,-19136,-37056,-54976,-72896,-90816,-108736,-126656,-144576,-162496,-180416,-198336};

__attribute__ ((noinline)) void 
foo ()
{
  int res = 0, i, j, k;

  for (k = 0; k < K; k++)
    {
      res = 1000000;
      for (j = 0; j < K; j++) 
        for (i = 0; i < K; i++) 
          res -= in[i+k][j] * coeff[i][j];
 
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
        in[i][j] = i+j;

      for (i = 0; i < K; i++)
        coeff[i][j] = i+2;
    }

  foo();

  for (k = 0; k < K; k++)
    if (out[k] != check_result[k])
      abort ();

  return 0;
}
        
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
      
