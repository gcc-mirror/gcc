/* { dg-require-effective-target vect_int_mult } */

#include <stdarg.h>
#include "tree-vect.h"

#define K 32

signed short in[2*K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
signed short coeff[K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int out[K];
int check_result[K] = {642816,660736,678656,696576,714496,732416,750336,768256,786176,804096,822016,839936,857856,875776,893696,911616,929536,947456,965376,983296,1001216,1019136,1037056,1054976,1072896,1090816,1108736,1126656,1144576,1162496,1180416,1198336};

__attribute__ ((noinline)) void 
foo ()
{
  int sum = 0, i, j, k;

  for (k = 0; k < K; k++)
    {
      sum = 0;
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

/* Vectorization of loops with multiple types and double reduction is not 
   supported yet.  */       
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail { ! aarch64*-*-* } } } } */
      
