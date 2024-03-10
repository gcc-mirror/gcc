/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-peeling-for-alignment=0 -fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
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
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
