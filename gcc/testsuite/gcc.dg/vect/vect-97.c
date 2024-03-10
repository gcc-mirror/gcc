/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
 
char x[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
char cb[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

__attribute__ ((noinline))
int main1 ()
{  
  struct {
    char *p;
    char *q;
  } s;
  int i;

  /* Check that datarefs analysis can determine that the access via pointer
     s.p is based off array x, which enables us to antialias this access from
     the access to array cb.  */
  s.p = x;
  for (i = 0; i < N; i++)
    {
      s.p[i] = cb[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.p[i] != cb[i])
        abort ();
    }

  /* Check that datarefs analysis can determine that the access via pointer
     s.p is based off array x, and that the access via pointer s.q is based off
     array cb, which enables us to antialias these two accesses.  */
  s.q = cb;
  for (i = 0; i < N; i++)
    {
      s.p[i] = s.q[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.p[i] != s.q[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 


/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
