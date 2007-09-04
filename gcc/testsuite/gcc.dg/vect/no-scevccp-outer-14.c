/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

__attribute__ ((noinline)) unsigned short
foo (short scale){
  int i;
  unsigned short j;
  unsigned short sum = 0;
  unsigned short sum_j;

  for (i = 0; i < N; i++) {
    sum_j = 0;
    for (j = 0; j < N; j++) {
      sum_j += j;
    }
    sum += sum_j;
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
  unsigned short sum = 0;
  unsigned short res;

  check_vect ();

  res = foo (2);

  /* check results:  */
  for (i=0; i<N; i++)
    {
      sum_j = bar();
      sum += sum_j;
    }
  if (res != sum)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { target vect_widen_mult_hi_to_si } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
