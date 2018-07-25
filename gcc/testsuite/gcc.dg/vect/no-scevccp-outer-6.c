/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

__attribute__ ((noinline)) int
foo (int * __restrict__ b, int k){
  int i,j;
  int sum,x;
  int a[N];

  for (i = 0; i < N; i++) {
    sum = b[i];
    for (j = 0; j < N; j++) {
      sum += j;
    }
    a[i] = sum;
  }
  
  return a[k];
}

int main (void)
{
  int i,j;
  int sum;
  int b[N];
  int a[N];

  check_vect ();

  for (i=0; i<N; i++)
    b[i] = i + 2;

  for (i=0; i<N; i++)
    a[i] = foo (b,i);

    /* check results:  */
  for (i=0; i<N; i++)
    {
      sum = b[i];
      for (j = 0; j < N; j++){
        sum += j;
      }
      if (a[i] != sum)
        abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail { unaligned_stack || { vect_no_align && { ! vect_hw_misalign } } } } } } */
/* Exclude widening of i * 4 in the calculation of a[i].  */
/* { dg-final { scan-tree-dump-times {vect_recog_widen_mult_pattern: detected: [^\n]* \* ([^4]|4[^;])} 1 "vect" { xfail *-*-* } } } */
