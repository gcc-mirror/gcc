/* { dg-require-effective-target vect_float } */
/* { dg-add-options double_vectors } */

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
	abort();
    }

  return;
}

/* Aligned pointer accesses.
   The loop bound is unknown.
   No aliasing problems.
   vect-40.c is similar to this one with one difference:
        the loop bound is known.  */

float b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)))
     = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)))
     = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

__attribute__ ((noinline)) int
main1 (int n)
{
  int i;
  float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float *pa = a;
  float *pb = b;
  float *pc = c;

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
  int n=N;
  check_vect ();
  main1 (n);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { xfail { ! vect_align_stack_vars } } } } */
