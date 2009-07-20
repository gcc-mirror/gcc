/* { dg-require-effective-target vect_condition } */

#include <stdlib.h> 
#include <stdio.h> 
#include "tree-vect.h"

#define M 32
#define N 16

int x_in[M];
int x_out[M];
int c[N] = {3,2,1,10,1,42,3,4,50,9,32,8,11,10,1,2};
int a[N+1] = {0,16,32,48,64,128,256,512,0,16,32,48,64,128,256,512,1024};
int check_result[M] = {1024,1024,1024,256,256,256,256,256,256,256,256,128,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48};

__attribute__ ((noinline)) void 
foo ()
{
  int j, i, x;
  int curr_a, next_a;

  for (j = 0; j < M; j++) 
    {
      x = x_in[j];
      curr_a = a[0];

      for (i = 0; i < N; i++) 
        {
          next_a = a[i+1];
          curr_a = x > c[i] ? curr_a : next_a;
        }

      x_out[j] = curr_a;
    }
}

int main (void)
{
  int i,j;

  check_vect ();

  for (j = 0; j < M; j++) 
    x_in[j] = j;

  foo ();

  for (j = 0; j < M; j++)
    if (x_out[j] != check_result[j])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */


