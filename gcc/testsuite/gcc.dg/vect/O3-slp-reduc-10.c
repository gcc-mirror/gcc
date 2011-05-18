/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
#define TYPE int
#define RESULT 755918

__attribute__ ((noinline)) TYPE fun2 (TYPE *x, TYPE *y, unsigned int n)
{
  int i, j;
  TYPE dot = 14;

  for (i = 0; i < n / 2; i++)
    for (j = 0; j < 2; j++)
      dot += *(x++) * *(y++);

  return dot;
}

int main (void)
{
  TYPE a[N], b[N], dot;
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i+8;
    }

  dot = fun2 (a, b, N);
  if (dot != RESULT)
    abort();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target { vect_int_mult && {! vect_no_align } } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
