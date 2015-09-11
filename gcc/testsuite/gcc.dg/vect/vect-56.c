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
      if (pa[i] != (pb[i+1] * pc[i+1]))
        abort ();
    }

  return;
}

/* Unaligned pointer read accesses, aligned write access.
   The loop bound is known and divisible by the vectorization factor.
   No aliasing problems.
   vect-48.c is similar to this one with one difference:
        the alignment of the read accesses is unknown.
   vect-60.c is similar to this one with one difference:
        the loop bound is unknown.
   vect-57.c is similar to this one with two differences:
        aliasing is a problem, and the write access has unknown alignment.  */

float b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float *pa = a;
  float *pb = b;
  float *pc = c;

  for (i = 0; i < N/2; i++)
    {
      pa[i] = pb[i+1] * pc[i+1];
    }

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (pa[i] != (pb[i+1] * pc[i+1]))
        abort ();
    }

  return 0;
}

int main (void)
{
  int i;

  check_vect ();
  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { xfail { vect_no_align || vect_element_align } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { vect_element_align } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" { xfail { vect_element_align } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { target { vect_element_align } } } } */
