/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

__attribute__ ((noinline)) int main1 (short X)
{
  signed char a[N];
  short b[N];
  int c[N];
  short myX = X;
  int i;

  /* vectorization of induction with type conversions.  */
  for (i = 0; i < N; i++)
  {
    a[i] = (signed char)X;
    b[i] = X;
    c[i] = (int)X;
    X++;
  }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (a[i] != (signed char)myX || b[i] != myX || c[i] != (int)myX++)
	abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 (3);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_pack_trunc && vect_unpack } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
