/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define A 3
#define N 16

short src[N], dst[N];

void foo (int a)
{
  dst[0] += a*src[0];
  dst[1] += a*src[1];
  dst[2] += a*src[2];
  dst[3] += a*src[3];
  dst[4] += a*src[4];
  dst[5] += a*src[5];
  dst[6] += a*src[6];
  dst[7] += a*src[7];
}


int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      dst[i] = 0;
      src[i] = i;
    }

  foo (A);

  for (i = 0; i < 8; i++)
    {
      if (dst[i] != A * i)
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "Vectorized basic-block" 1 "slp" { target { vect_int_mult && { vect_unpack && vect_pack_trunc } } } } } */
/* { dg-final { cleanup-tree-dump "slp" } } */

