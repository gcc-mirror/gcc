/* { dg-require-effective-target vect_float } */
/* { dg-add-options bind_pic_locally } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 20

float a[N];
float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

__attribute__ ((noinline)) int
main1 ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      a[i] = b[i] * c[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i <N; i++)
    {
      if (a[i] != b[i] * c[i])
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
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
