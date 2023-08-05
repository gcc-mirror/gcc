/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

int ic[N*2];
int ib[N];

#define ia (ic+N)

__attribute__ ((noinline))
int main1 ()
{
  int i, j;

  for (i = 0; i < N; i++)
    {
      ib[i] = i*3;
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N; i++)
    {
       ia[i] = ib[i];
    }

  /* check results: */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
       if (ia[i] != ib[i])
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
