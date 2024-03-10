/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

short sb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
int ia[N];

__attribute__ ((noinline))
int main1 ()
{
  int i;

  /* Requires type promotion (vector unpacking) support.  */
  for (i = 0; i < N; i++)
    {
      ia[i] = (int) sb[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ia[i] != (int) sb[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_unpack } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
