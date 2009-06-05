/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

extern int a[N];

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
      a[i] = a[i+20];
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 3 "vect" { xfail vect_no_int_add } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 6 "vect" { target vect_hw_misalign } } } */
/* { dg-final { scan-tree-dump-times "accesses have the same alignment." 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 3 "vect" {target { vector_alignment_reachable && {! vect_hw_misalign} } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 3 "vect" {target { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
