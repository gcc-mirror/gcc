/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

__attribute__ ((noinline))
int main1 (int n, int *p)
{
  int i;
  int ib[N];
  int ia[N];
  int k;

  for (i = 0; i < N; i++)
    {
      ia[i] = n;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != n)
        abort ();
    }

  k = *p;
  for (i = 0; i < N; i++)
    {
      ib[i] = k;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ib[i] != k)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  int m = 8;

  check_vect ();
  
  return main1 (m, &m);
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { xfail { ! vect_align_stack_vars } } } } */
