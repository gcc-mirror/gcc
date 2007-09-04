/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

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
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { {! vect_no_align} && vector_alignment_reachable } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { vect_no_align || {! vector_alignment_reachable} } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 1 "vect" { target { vect_no_align || {! vector_alignment_reachable} } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
