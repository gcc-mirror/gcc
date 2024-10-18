/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "--param vect-max-version-for-alias-checks=0" } */

#include <stdarg.h>
#include <string.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline))
void bar (float *pa, float *pb, float *pc)
{
  int i;

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N/2; i++)
    {
      if (pa[i] != (pb[i+1] * pc[i+1]))
        abort ();
    }

  return;
}

__attribute__ ((noinline))
void foo (float *pb, float *pc)
{
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};
  memcpy (pb, b, sizeof (b));
  memcpy (pc, c, sizeof (c));
}

/* Unaligned pointer read accesses with known alignment,
   and an unaligned write access with unknown alignment.
   The loop bound is known and divisible by the vectorization factor.
   Can't prove that the pointers don't alias.
   vect-61.c is similar to this one with one difference:
        the loop bound is unknown.
   vect-56.c is similar to this one with two differences:
        aliasing is a problem, and the write access is aligned.  */

__attribute__ ((noinline)) int
main1 (float *pa)
{
  int i;
  float b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float *pb = b;
  float *pc = c;

  foo (pb, pc);

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
  float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

  check_vect ();
  main1 (a);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
