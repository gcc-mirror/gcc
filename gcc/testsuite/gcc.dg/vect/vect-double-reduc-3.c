/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define K 32

int in[2*K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int coeff[K][K] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
int out_max[K], out_min[K];
int check_max[K] = {62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93};
int check_min[K] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};

__attribute__ ((noinline)) void 
foo (int x, int y)
{
  int max, min, i, j, k;

  for (k = 0; k < K; k++)
    {
      max = x;
      min = y;
      for (j = 0; j < K; j++) 
        for (i = 0; i < K; i++)
          {
            max = max < in[i+k][j] ? in[i+k][j] : max; 
            min = min > in[i+k][j] ? in[i+k][j] : min; 
          }
      out_max[k] = max;
      out_min[k] = min;
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

  foo(0, 0);

  for (k = 0; k < K; k++)
    if (out_max[k] != check_max[k] || out_min[k] != 0)
      abort ();

  foo(100, 45);

  for (k = 0; k < K; k++)
    if (out_min[k] != check_min[k] || out_max[k] != 100)
      abort ();

  return 0;
}
        
/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail vect_no_int_min_max } } } */
      
