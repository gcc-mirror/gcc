/* { dg-require-effective-target vect_double } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  float fb[N] = {0.4,3.5,6.6,9.4,12.5,15.6,18.4,21.5,24.6,27.4,30.5,33.6,36.4,39.5,42.6,45.4,0.5,3.6,6.4,9.5,12.6,15.4,18.5,21.6,24.4,27.5,30.6,33.4,36.5,39.6,42.4,45.5};
  double da[N];

  /* float -> double */
  for (i = 0; i < N; i++)
    {
      da[i] = (double) fb[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (da[i] != (double) fb[i])
       abort ();
    }

  return 0;
}

int
main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail spu*-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
