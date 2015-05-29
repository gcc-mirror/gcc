/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16 

float out[N];
float check_res[N] = {880,864,848,832,816,800,784,768,752,736,720,704,688,672,656,640};
float a[2*N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};

/* Outer-loop vectorization.  */

__attribute__ ((noinline)) void 
foo () 
{
  int i, j;
  float res;

  for (i = 0; i < N; i++)
    {
      res = 1000;

      for (j = 0; j < N; j++) 
        res = res - a[i+j];
        
      out[i] = res;
    }

  for (i = 0; i < N; i++)  
    if (out[i] != check_res[i])
      abort ();

}

int main ()
{
  check_vect ();

  foo();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail { vect_no_align && { ! vect_hw_misalign } } } } } */

