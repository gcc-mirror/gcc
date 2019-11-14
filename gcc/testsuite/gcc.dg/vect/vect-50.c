/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "--param vect-max-peeling-for-alignment=0" } */

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

__attribute__ ((noinline)) int
main1 (int n, float * __restrict__ pa, float * __restrict__ pb, float * __restrict__ pc)
{
  int i;

  for (i = 0; i < n; i++)
    {
      pa[i] = pb[i] * pc[i];
    }

  bar (pa,pb,pc);

  return 0;
}

/* Unaligned pointer accesses, with unknown alignment.
   The loop bound is unknown.
   No aliasing problems.
   vect-44.c is similar to this one with one difference:
        the loop bound is known.  
   vect-51.c is similar to this one with one difference:
        can't prove that pointers don't alias.  */

int main (void)
{
  int i;
  float a[N];
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (N,a,b,c);
  return 0;
}

/* For targets that don't support misaligned loads and don't support
   misaligned stores we version for the all three accesses (peeling to
   align the store will not force the two loads to be aligned).  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 3 "vect" { xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" { xfail { { vect_no_align && { ! vect_hw_misalign } } || {! vector_alignment_reachable} } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 3 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 1 "vect" { target { {! vector_alignment_reachable} && { {! vect_no_align } && {! vect_hw_misalign } } } } } } */
