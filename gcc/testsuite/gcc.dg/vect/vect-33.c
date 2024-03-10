/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
struct test {
  char ca[N];
};

extern struct test s;
 
__attribute__ ((noinline))
int main1 ()
{  
  int i;

  for (i = 0; i < N; i++)
    {
      s.ca[i] = 5;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != 5)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump "Vectorizing an unaligned access" "vect" { target { { { ! powerpc*-*-* } && vect_hw_misalign } && { { ! vect64 } || vect_multiple_sizes } } xfail { ! vect_unaligned_possible } } } }  */
/* { dg-final { scan-tree-dump "Alignment of access forced using peeling" "vect" { target { vector_alignment_reachable && { vect64 && {! vect_multiple_sizes} } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { { {! vector_alignment_reachable} || {! vect64} } && {! vect_hw_misalign} } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
