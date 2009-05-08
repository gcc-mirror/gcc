/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline))
void bar (const float *pa, const float *pb, const float *pc) 
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
   The loop bound is unknown.
   Can't prove that the pointers don't alias.
   vect-49.c is similar to this one with one difference:
        the loop bound is known.
   vect-52.c is similar to this one with one difference:
        aliasing is not a problem.  */

__attribute__ ((noinline)) int
main1 (int n, float *pb, float *pc)
{
  float pa[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  int i;

  for (i = 0; i < n; i++)
    {
      pa[i] = pb[i] * pc[i];
    }

  bar (pa,pb,pc);

  return 0;
}

int main (void)
{
  int i;
  float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float b[N+1] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60};
  float c[N+1] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};

  check_vect ();

  main1 (N,&b[1],c);
  main1 (N,&b[1],&c[1]);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
