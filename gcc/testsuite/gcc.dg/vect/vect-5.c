/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

int main1 ()
{
  int i, j;
  float a[N];
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  float d[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};

  i = 0;
  j = 0;
  while (i < 5*N)
    {
      a[j] = c[j];
      i += 5;
      j++;
    }

  /* check results:  */
  for (i = 0; i <N; i++)
    {
      if (a[i] != c[i])
        abort ();
    }


  for (i = N; i > 0; i--)
    {
      a[N-i] = d[N-i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (a[i] != d[i])
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
