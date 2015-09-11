/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

float fa[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float fb[N+4] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float fc[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

/* Check handling of accesses for which the "initial condition" -
   the expression that represents the first location accessed - is
   more involved than just an ssa_name.  */

volatile int y = 0;

__attribute__ ((noinline)) int
main1 (float * __restrict__ pa, float * __restrict__ pb, float *__restrict__ pc)
{
  int i;
  float *q = pb + 4;
  for (i = 0; i < N; i++)
    {
      fb[i] = i;
      fc[i] = 0.5+i;
      if (y)
	abort ();
    }
  for (; i < N+4; i++)
    {
      fb[i] = i;
      if (y)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      pa[i] = q[i] * pc[i];
    }

  for (i = 0; i < N; i++)
    {
      if (pa[i] != q[i] * pc[i])
	abort ();
    }

  return 0;
}


int main (void)
{
  check_vect ();

  main1 (fa, fb, fc);

  return 0;
}

/* For targets that don't support misaligned loads we version for the
   all three accesses (peeling to align the store will not force the
   two loads to be aligned).  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* Uncomment when this testcase gets vectorized again:
 dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { xfail vect_no_align } }
 dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail vect_no_align } }
 dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 3 "vect" { target vect_no_align } }
*/
