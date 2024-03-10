/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

int a[N];
int b[N];
int c[N];

__attribute__ ((noinline)) int
foo (){
  int i;
  unsigned short j;
  int sum = 0;
  unsigned short sum_j;

  for (i = 0; i < N; i++) {
    int diff = b[i] - c[i];

    sum_j = 0;
    for (j = 0; j < N; j++) {
      sum_j += j;
    }
    a[i] = sum_j + 5;

    sum += diff;
  }
  return sum;
}

int main (void)
{
  int i;
  unsigned short j, sum_j;
  int sum = 0;
  int res;

  check_vect ();

  for (i=0; i<N; i++){
    b[i] = i;
    c[i] = 2*i;
  }
 
  res = foo ();

  /* check results:  */
#pragma GCC novector
  for (i=0; i<N; i++)
    {
      sum += (b[i] - c[i]);

      sum_j = 0;
      for (j = 0; j < N; j++){
        sum_j += j;
      }
      if (a[i] != sum_j + 5)
        abort();
    }
  if (res != sum)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail { ! {vect_unpack } } } } } */
