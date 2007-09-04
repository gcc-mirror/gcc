/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

unsigned short in[N];

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
    sum += ((unsigned int) in[i] * (unsigned int) sum_j) >> scale;
  }
  return sum;
}

__attribute__ ((noinline)) unsigned short
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
    in[i] = i;
  }
 
  res = foo (2);

  /* check results:  */
  for (i=0; i<N; i++)
    {
      sum_j = bar ();
      sum += ((unsigned int) in[i] * (unsigned int) sum_j) >> 2;
    }
  if (res != sum)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
