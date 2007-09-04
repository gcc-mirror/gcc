/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline))
void bar (float *pa, float *pb, float *pc) 
{
  int i;

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] * pc[i]))
	abort ();
    }

  return;
}

/* Unaligned pointer read accesses, aligned pointer write access.
   The loop bound is known and divisible by the vectorization factor.
   Can't prove that the pointers don't alias.
   vect-53.c is similar to this one with one difference:
        the loop bound is unknown.
   vect-48.c is similar to this one with one difference:
        aliasing is not a problem.  */

__attribute__ ((noinline)) int
main1 (float *pb, float *pc)
{
  float pa[N] __attribute__ ((__aligned__(16)));
  int i;

  for (i = 0; i < N; i++)
    {
      pa[i] = pb[i] * pc[i];
    }

  bar (pa,pb,pc);

  return 0;
}

int main (void)
{
  int i;
  float b[N+1] __attribute__ ((__aligned__(16))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60};
  float c[N] __attribute__ ((__aligned__(16))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (b,c);
  main1 (&b[1],c);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
