/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-peeling-for-alignment=0" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
#define OFF 3

/* unaligned store.  */

__attribute__ ((noinline))
int main1 (int off)
{
  int i;
  int ia[N+OFF];

  for (i = 0; i < N; i++)
    {
      ia[i+off] = 5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i+off] != 5)
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } } */
