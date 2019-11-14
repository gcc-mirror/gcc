/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */
/* { dg-add-options double_vectors } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

/* Unaligned pointer read accesses, aligned write access.
   The loop bound is known and divisible by the vectorization factor.
   No aliasing problems.
   vect-56.c is similar to this one with one difference:
        the alignment of the read accesses is known.
   vect-52.c is similar to this one with one difference:
        the loop bound is unknown.
   vect-49.c is similar to this one with one difference:
        aliasing is a problem.  */

__attribute__ ((noinline)) int
main1 (float *pb, float *pc)
{
  float pa[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  int i;

  for (i = 0; i < N; i++)
    {
      pa[i] = pb[i] * pc[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] * pc[i]))
	abort ();
    }

  return 0;
}

int main (void)
{
  int i;
  float b[N+1] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60};
  float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (b,c);
  main1 (&b[1],c);

  return 0;
}

/* For targets that don't support misaligned loads we version for the two loads.
   (The store is aligned).  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 3 "vect" { target { ! vect_align_stack_vars } xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { target vect_align_stack_vars xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 2 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
