/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int
main1 (void)
{
  int i;
  float a[N];
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

  /* Condition in loop.  */
  /* This loop is vectorized on platforms that support vect_condition.  */
  for (i = 0; i < N; i++)
  {
    a[i] = (b[i] > 0 ? b[i] : 0);
  }

  for (i = 0; i < N; i++)
  {
    if (a[i] != b[i])
	abort ();
  }
  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target powerpc*-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target i?86-*-* x86_64-*-* ia64-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
