/* { dg-require-effective-target vect_int_mult } */

#include <stdarg.h>
#include "tree-vect.h"

#define K 32

int in[2*K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int coeff[K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int out[K];
int check_result[K] = {652816,670736,688656,706576,724496,742416,760336,778256,796176,814096,832016,849936,867856,885776,903696,921616,939536,957456,975376,993296,1011216,1029136,1047056,1064976,1082896,1100816,1118736,1136656,1154576,1172496,1190416,1208336};

__attribute__ ((noinline)) void 
foo ()
{
  int sum = 0, i, j, k;

  for (k = 0; k < K; k++)
    {
      sum = 10000;
      for (j = 0; j < K; j++) 
        for (i = 0; i < K; i++) 
          sum += in[i+k][j] * coeff[i][j];
 
      out[k] = sum;
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
      
