/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 40

int a[N];

__attribute__ ((noinline)) int
foo (){
  int i;
  unsigned short j;
  int sum = 0;
  unsigned short sum_j;

  for (i = 0; i < N; i++) {
    sum += i;

    sum_j = i;
    for (j = 0; j < N; j++) {
      sum_j += j;
    }
    a[i] = sum_j + 5;
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

  for (i=0; i<N; i++)
    a[i] = i;
 
  res = foo ();

  /* check results:  */
  for (i=0; i<N; i++)
    {
      sum += i;

      sum_j = i;
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

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail { ! { vect_pack_trunc } } } } } */
