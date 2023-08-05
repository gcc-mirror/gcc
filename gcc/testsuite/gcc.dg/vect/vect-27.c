/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0 -fdump-tree-optimized-details-blocks" } */
/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

/* unaligned load.  */

int ia[N];
int ib[N+1];

__attribute__ ((noinline))
int main1 ()
{
  int i;

  for (i=0; i <= N; i++)
    {
      ib[i] = i;
    }

  for (i = 1; i <= N; i++)
    {
      ia[i-1] = ib[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 1; i <= N; i++)
    {
      if (ia[i-1] != ib[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* The initialization induction loop (with aligned access) is also vectorized.  */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { xfail { ! vect_unaligned_possible } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
