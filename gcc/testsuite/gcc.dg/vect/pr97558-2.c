/* { dg-do run } */
/* { dg-additional-options "-O3 -fno-tree-forwprop -fno-tree-scev-cprop" } */

#include "tree-vect.h"

#define N 40

int a[N];
int b[N];

__attribute__ ((noinline)) int
foo (int n){
  int i,j;
  int sum,x,y;

  if (n<=0)
    return 0;

  for (i = 0; i < N/2; i++) {
    sum = 0;
    x = b[2*i];
    y = b[2*i+1];
    for (j = 0; j < n; j++) {
      sum += j;
    }
    a[2*i] = sum + x;
    a[2*i+1] = sum + y;
  }
}

int main (void)
{
  int i,j;
  int sum;

  check_vect ();

  for (i=0; i<N; i++)
    b[i] = i;
 
  foo (N-1);

    /* check results:  */
#pragma GCC novector
  for (i=0; i<N/2; i++)
    {
      sum = 0;
      for (j = 0; j < N-1; j++)
        sum += j;
      if (a[2*i] != sum + b[2*i] || a[2*i+1] != sum + b[2*i+1])
        abort();
    }

  return 0;
}
