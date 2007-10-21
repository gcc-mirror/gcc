/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

unsigned short a[N];
unsigned int b[N];

__attribute__ ((noinline)) int
foo (){
  unsigned short i,j;
  unsigned short sum;

  for (i = 0; i < N; i++) {
    sum = 0;
    for (j = 0; j < N; j++) {
      sum += j;
    }
    a[i] = sum;
    b[i] = (unsigned int)sum;
  }
}

int main (void)
{
  int i,j;
  short sum;

  check_vect ();

  for (i=0; i<N; i++)
    a[i] = i;
 
  foo ();

    /* check results:  */
  for (i=0; i<N; i++)
    {
      sum = 0;
      for (j = 0; j < N; j++)
        sum += j;
      if (a[i] != sum  || b[i] != (unsigned int)sum)
        abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED." 1 "vect" { xfail { ! {vect_unpack } } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
