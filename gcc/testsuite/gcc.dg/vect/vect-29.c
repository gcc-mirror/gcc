/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0 -fdump-tree-optimized-details-blocks" } */
/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
#define OFF 3

/* unaligned load.  */

int ia[N];
int ib[N+OFF];

__attribute__ ((noinline))
int main1 (int off)
{
  int i;

  for (i = 0; i < N+OFF; i++)
    {
      ib[i] = i;
    }

  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i+off];
    }

  /* check results:  */
#pragma GCC novector
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
  
  main1 (0); /* aligned */
  main1 (OFF); /* unaligned */
  return 0;
}

/* For targets that don't support misaligned loads we version for the load.
   (The store is aligned).  */

/* The initialization induction loop (with aligned access) is also vectorized.  */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning." 1 "vect" {target { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
