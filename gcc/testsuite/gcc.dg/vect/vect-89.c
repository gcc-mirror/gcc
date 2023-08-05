/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0 -fdump-tree-optimized-details-blocks" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 256
#define N (VECTOR_BITS * 2 / 32)
#else
#define N 16
#endif

struct tmp_struct
{
  int x;
  int y[N];
};
	 
__attribute__ ((noinline))
int main1 ()
{  
  int i, *q;
  struct tmp_struct tmp, *p;

  p = &tmp;
  q = p->y;

  for (i = 0; i < N; i++)
    {
      *q++ = 5;
    }

  /* check results: */  
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (p->y[i] != 5)
        {
          abort ();
        }
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
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { vect_element_align_preferred || { ! vect_align_stack_vars } } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
