/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0 -fdump-tree-optimized-details-blocks" } */
/* { dg-require-effective-target vect_int } */
/* { dg-add-options double_vectors } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 256
#define N (VECTOR_BITS * 2 / 32)
#else
#define N 16
#endif

struct tmp
{
     int x;
     int ia[N];
};

__attribute__ ((noinline))
int main1 (int off)
{
  struct tmp sb[N];
  struct tmp *pp = &sb[off];
  int i, ib[N];

  for (i = 0; i < N; i++)
      pp->ia[i] = ib[i];

  /* check results: */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
       if (pp->ia[i] != ib[i])
         abort();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 (8);
}

/* The store is unaligned, the load is aligned. For targets that support unaligned
   loads, peel to align the store and generate an unaligned access for the load.
   For targets that don't support unaligned loads, version for the store.  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { target { { {! vect_no_align} && vector_alignment_reachable } && { ! vect_align_stack_vars } } xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { { {! vect_no_align} && vector_alignment_reachable } && vect_align_stack_vars } xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { { vect_no_align } || { { ! vector_alignment_reachable} || vect_element_align } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 1 "vect" { target { { vect_no_align && { ! vect_hw_misalign } } || { {! vector_alignment_reachable} && {! vect_element_align} } } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
