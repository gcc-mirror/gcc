/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS / 8)
#else
#define N 16
#endif

__attribute__ ((noinline))
int main1 ()
{  
  struct {
    char ca[N];
  } s;
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
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { xfail { ! vect_align_stack_vars } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
