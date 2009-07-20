/* { dg-require-effective-target vect_condition } */

#include <stdlib.h> 
#include <stdio.h> 
#include "tree-vect.h"

#define M 32
#define N 16

int x_in[M];
int x_out_a[M], x_out_b[M];
int c[N] = {3,2,1,10,1,42,3,4,50,9,32,8,11,10,1,2};
int a[N+1] = {0,16,32,48,64,128,256,512,0,16,32,48,64,128,256,512,1024};
int b[N+1] = {17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1};
int check_result_a[M] = {1024,1024,1024,256,256,256,256,256,256,256,256,128,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48};
int check_result_b[M] = {17,17,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

__attribute__ ((noinline)) void 
foo (int z)
{
  int j, i, x;
  int curr_a, flag, next_a, curr_b, next_b;

  for (j = 0; j < M; j++) 
    {
      x = x_in[j];
      curr_a = a[0];
      curr_b = b[0];

      for (i = 0; i < N; i++) 
        {
          curr_a = x > c[i] ? curr_a : z;
          curr_b = x > c[i] ? next_b : 5;
        }

      x_out_a[j] = curr_a;
      x_out_b[j] = curr_b;
    }
}

int main (void)
{
  int i,j;

  check_vect ();

  for (j = 0; j < M; j++) 
    x_in[j] = j;

  foo (125);

  for (j = 0; j < M; j++)
    if (x_out_a[j] != 125
        || x_out_b[j] != 5)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { xfail vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */


