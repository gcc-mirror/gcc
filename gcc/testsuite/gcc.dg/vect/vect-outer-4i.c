/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 96
#define M 128
unsigned char in[N+M];
unsigned short out[N];

/* Outer-loop vectorization. */
/* Multiple-types in the inner-loop.  */

__attribute__ ((noinline))
unsigned short
foo (){
  int i,j;
  unsigned short diff;
  unsigned short s=0;

  for (i = 0; i < N; i++) {
    diff = 0;
    for (j = 0; j < M; j+=8) {
      diff += in[j+i];
    }
    s+=diff;
  }
  return s;
}

int main (void)
{
  check_vect ();
  int i;
  unsigned short s;

  for (i = 0; i < N+M; i++)
    in[i] = (unsigned char)i;

  s = foo ();

  if (s != 34048)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" { target vect_unpack } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
