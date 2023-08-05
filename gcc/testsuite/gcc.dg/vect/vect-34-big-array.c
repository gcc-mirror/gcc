/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

struct {
  char ca[N];
} s;
char cb[N];

__attribute__ ((noinline))
int main1 ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      cb[i] = i*3;
      asm volatile ("" ::: "memory");
    }
  for (i = 0; i < N; i++)
    {
      s.ca[i] = cb[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != cb[i])
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
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
