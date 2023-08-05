/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

__attribute__ ((noinline))
int main1 ()
{
  int i;
  int a[N];
  int b[N];

  for (i = 0; i <N; i++)
    {
      b[i] = i*3;
      asm volatile ("" ::: "memory");
    }

  /* Not vectorizable yet (reverse access and forward access).  */
  for (i = N; i > 0; i--)
    {
      a[N-i] = b[i-1];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i <N; i++)
    {
      if (a[i] != b[N-1-i])
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_perm && vect_hw_misalign } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
