/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

void bar (float *pa, float *pb, float *pc)
{
  int i;

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (pa[i] != (pb[i+1] * pc[i+1]))
        abort ();
    }

  return;
}

/* Unaligned pointer read accesses with known alignment,
   and an unaligned write access with unknown alignment.
   The loop bound is known and divisible by the vectorization factor.
   Can't prove that the pointers don't alias.
   vect-61.c is similar to this one with one difference:
        the loop bound is unknown.
   vect-56.c is similar to this one with two differences:
        aliasing is a problem, and the write access is aligned.  */

int
main1 (float *pa)
{
  int i;
  float b[N] __attribute__ ((__aligned__(16))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float c[N] __attribute__ ((__aligned__(16))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};
  float *pb = b;
  float *pc = c;

  for (i = 0; i < N/2; i++)
    {
      pa[i] = pb[i+1] * pc[i+1];
    }

  bar (pa, pb, pc);

  return 0;
}

int main (void)
{
  int i;
  float a[N] __attribute__ ((__aligned__(16)));

  check_vect ();
  main1 (a);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
