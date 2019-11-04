/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline))
void bar (float *pa, float *pb, float *pc)
{
  int i;

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (pa[i+1] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return;
}

/* Unaligned pointer accesses, with a known alignment.
   The loop bound is unknown.
   No aliasing problems.
   vect-54.c is similar to this one with one difference:
        the loop bound is known.  */

float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));  float b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};  float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

__attribute__ ((noinline)) int
main1 (int n)
{
  int i;
  float *pa = a;
  float *pb = b;
  float *pc = c;

  for (i = 0; i < n/2; i++)
    {
      pa[i+1] = pb[i+1] * pc[i+1];
    }

  bar (a,b,c);

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
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail vect_element_align_preferred } } } */
