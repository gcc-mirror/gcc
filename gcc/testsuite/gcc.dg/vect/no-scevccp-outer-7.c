/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param max-completely-peel-times=1" } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 256
#define N (VECTOR_BITS / 16)
#else
#define N 16
#endif

unsigned short in[N];
unsigned short coef[N];
unsigned short a[N];

__attribute__ ((noinline)) unsigned int
foo (short scale){
  int i;
  unsigned short j;
  unsigned int sum = 0;
  unsigned short sum_j;

  for (i = 0; i < N; i++) {
    sum_j = 0;
    for (j = 0; j < N; j++) {
      sum_j += j;
    }
    a[i] = sum_j;
    sum += ((unsigned int) in[i] * (unsigned int) coef[i]) >> scale;
  }
  return sum;
}

unsigned short
bar (void)
{
  unsigned short j;
  unsigned short sum_j;

  sum_j = 0;
  for (j = 0; j < N; j++) {
    sum_j += j;
  }

  return sum_j;
}

int main (void)
{
  int i;
  unsigned short j, sum_j;
  unsigned int sum = 0;
  unsigned int res;

  check_vect ();

  for (i=0; i<N; i++){
    in[i] = 2*i;
    coef[i] = i;
  }
 
  res = foo (2);

  /* check results:  */
#pragma GCC novector
  for (i=0; i<N; i++)
    {
      if (a[i] != bar ())
	abort ();
      sum += ((unsigned int) in[i] * (unsigned int) coef[i]) >> 2;
    }
  if (res != sum)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_widen_mult_pattern: detected(?:(?!Analysis failed).)*Analysis succeeded" 1 "vect" { target vect_widen_mult_hi_to_si } } } */
