/* { dg-require-effective-target vect_double } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

_Complex double a[N] =
    { 10.0F + 20.0iF, 11.0F + 21.0iF, 12.0F + 22.0iF, 13.0F + 23.0iF,
      14.0F + 24.0iF, 15.0F + 25.0iF, 16.0F + 26.0iF, 17.0F + 27.0iF,
      18.0F + 28.0iF, 19.0F + 29.0iF, 20.0F + 30.0iF, 21.0F + 31.0iF,
      22.0F + 32.0iF, 23.0F + 33.0iF, 24.0F + 34.0iF, 25.0F + 35.0iF };
_Complex double b[N] =
    { 30.0F + 40.0iF, 31.0F + 41.0iF, 32.0F + 42.0iF, 33.0F + 43.0iF,
      34.0F + 44.0iF, 35.0F + 45.0iF, 36.0F + 46.0iF, 37.0F + 47.0iF,
      38.0F + 48.0iF, 39.0F + 49.0iF, 40.0F + 50.0iF, 41.0F + 51.0iF,
      42.0F + 52.0iF, 43.0F + 53.0iF, 44.0F + 54.0iF, 45.0F + 55.0iF };

_Complex double c[N];
_Complex double res[N] =
    { 40.0F + 60.0iF, 42.0F + 62.0iF, 44.0F + 64.0iF, 46.0F + 66.0iF,
      48.0F + 68.0iF, 50.0F + 70.0iF, 52.0F + 72.0iF, 54.0F + 74.0iF,
      56.0F + 76.0iF, 58.0F + 78.0iF, 60.0F + 80.0iF, 62.0F + 82.0iF,
      64.0F + 84.0iF, 66.0F + 86.0iF, 68.0F + 88.0iF, 70.0F + 90.0iF };


__attribute__ ((noinline)) void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    c[i] = a[i] + b[i];

}

int
main (void)
{ 
  int i;
  check_vect ();
  
  foo ();

  /* check results:  */
  for (i = 0; i < N; i++)
    if (c[i] != res[i])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
