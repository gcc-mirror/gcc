/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
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
	abort ();
    }

  return;
}

/* Unaligned write access, aligned read accesses.
   Since we are handling an unaligned store by peeling the loop,
   the loads will become unaligned.
   The loop bound is known and divisible by the vectorization factor.
   No aliasing problems.  */

__attribute__ ((noinline)) int
main1 (float * __restrict__ pa, float *pb, float *pc)
{
  float b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  int i;

  /* We also vectorize this loop.  */
  for (i = 0; i < N; i++)
    {
      b[i] = pb[i];
      c[i] = pc[i];
    }

  for (i = 0; i < N; i++)
    {
      pa[i] = b[i] * c[i];
    }

  return 0;
}

int main (void)
{
  int i;
  float a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (a,b,c);
  bar (a,b,c);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 3 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { { ! vector_alignment_reachable } && { ! vect_element_align } } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 4 "vect" { xfail { vect_no_align || { { !  vector_alignment_reachable } || vect_element_align  } } } } }  */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 3 "vect" { target vect_element_align xfail { ! { vect_unaligned_possible && vect_align_stack_vars } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { vect_no_align || { { ! vector_alignment_reachable } || vect_element_align } } } } } */
