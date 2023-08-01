/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0 -fdump-tree-optimized-details-blocks" } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-peeling-for-alignment=0" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

/* Pick a value greater than the vector length.  */
#if VECTOR_BITS > 128
#define OFF (VECTOR_BITS * 5 / 32)
#else
#define OFF 20
#endif

extern int a[N + OFF];

/* The alignment of 'pa' is unknown. 
   Yet we do know that both the read access and write access have 
   the same alignment. Peeling to align one of the accesses will 
   align the other.  */

__attribute__ ((noinline)) int
main1 (int * pa)
{
  int i;

  for (i = 0; i < N; i++)
    {
      pa[i] = pa[i] + 1;
    }

  return 0;
}

/* The alignment of 'a' is unknown. 
   Yet we do know that both the read access and write access have 
   the same alignment. Peeling to align one of the accesses will 
   align the other.  */

__attribute__ ((noinline)) int
main2 ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      a[i] = a[i] + 1;
    }

  return 0;
}

__attribute__ ((noinline)) int
main3 ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      a[i] = a[i + OFF];
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 3 "vect" { xfail vect_no_int_add } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 3 "vect" {target { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
