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

/* Unaligned pointer accesses, with unknown alignment.
   The loop bound is known and divisible by the vectorization factor.
   No aliasing problems.
   vect-50.c is similar to this one with one difference:
        the loop bound is unknown.
   vect-45.c is similar to this one with one difference:
        can't prove that pointers don't alias.  */

__attribute__ ((noinline)) int
main1 (float * __restrict__ pa, float * __restrict__ pb, float * __restrict__ pc)
{
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
  float a[N+4] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float b[N+4] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69};
  float c[N+4] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23};

  check_vect ();

  main1 (a,b,c);
  main1 (&a[1],b,c);
  main1 (a,&b[1],c);
  main1 (&a[1],&b[1],&c[1]);

  return 0;
}

/* For targets that don't support misaligned loads we version for the 
   all three accesses (peeling to align the store will not force the
   two loads to be aligned).  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 3 "vect" { xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" { xfail { { vect_no_align && { ! vect_hw_misalign } } || {! vector_alignment_reachable} } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 3 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 1 "vect" { target { {! vector_alignment_reachable} && {{! vect_no_align} && {! vect_hw_misalign} } } } } } */
