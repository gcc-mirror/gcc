/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int main1 (short a, short *b)
{
  while (++a < 4) *b++ = 2;

  return 0;
}

int main (void)
{
  int i = 0;
  short x[N];

  check_vect ();

  main1 (0, x);

  /* check results:  */
  while (++i < 4)
    {
      if (x[i-1] != 2)
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
