/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 2 / 32)
#define OFF (VECTOR_BITS / 32)
#else
#define N 8
#define OFF 8
#endif

/* Check handling of accesses for which the "initial condition" -
   the expression that represents the first location accessed - is
   more involved than just an ssa_name.  */

int ib[N+OFF] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0, 1, 3, 5, 7, 11, 13, 17, 0, 2, 6, 10, 14, 22, 26, 34};
int off = 8;

__attribute__ ((noinline))
int main1 (int *ib)
{
  int i;
  int ia[N];

  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i+off];
    }


  /* check results:  */
  for (i = 0; i < N; i++)
    {
     if (ia[i] != ib[i+off])
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  main1 (ib);
  return 0;
}

/* For targets that don't support misaligned loads we version for the load.
   The store is aligned if alignment can be forced on the stack. Otherwise, we need to 
   peel the loop in order to align the store. For targets that can't align variables
   using peeling (don't guarantee natural alignment) versioning the loop is required
   both for the load and the store.  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target vect_align_stack_vars xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { target { ! vect_align_stack_vars } xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { target { {! vect_no_align} && { unaligned_stack && vector_alignment_reachable } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 1 "vect" { target { { {! unaligned_stack} && { vect_no_align && { ! vect_hw_misalign } } } || {unaligned_stack && { {! vector_alignment_reachable} && { ! vect_no_align } } } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 2 "vect" { target { { unaligned_stack && { vector_alignment_reachable && vect_no_align } } || {unaligned_stack && { {! vector_alignment_reachable} && vect_no_align } } } } } } */
