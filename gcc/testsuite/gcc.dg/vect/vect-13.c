/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int a[N];
int results[N] = {0,1,2,3,0,0,0,0,0,0,0,0,12,13,14,15};
int b[N] = {0,1,2,3,-4,-5,-6,-7,-8,-9,-10,-11,12,13,14,15};

__attribute__ ((noinline))
int main1()
{
  int i;

  /* Max pattern.  */
  for (i = 0; i < N; i++)
    {
      a[i] = (b[i] >= 0 ? b[i] : 0);
    }

  /* Check results  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (a[i] != results[i])
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_int_min_max } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
