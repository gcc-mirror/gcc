/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

__attribute__ ((noinline))
int main1 ()
{
  union {
    unsigned char a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
    unsigned char b[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  } s;
  int i;

  /* Initialization.  */
  for (i = 0; i < N; i++)
    {
      s.b[i] = i;
    }

  for (i = 0; i < N; i++)
    {
      s.a[i] = s.b[i] + 1;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.a[i] != i + 1)
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}


/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect"  { xfail { ia64-*-* sparc*-*-* } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
