/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#include "tree-vect.h"

#define N 111

/* This shouldn't be treated as an over-widening operation: it's better
   to reuse the extensions of di and ei for di + ei than to add them
   as shorts and introduce a third extension.  */

void __attribute__ ((noipa))
f (unsigned int *restrict a, unsigned int *restrict b,
   unsigned int *restrict c, unsigned char *restrict d,
   unsigned char *restrict e)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      unsigned int di = d[i];
      unsigned int ei = e[i];
      a[i] = di;
      b[i] = ei;
      c[i] = di + ei;
    }
}

int
main (void)
{
  check_vect ();

  unsigned int a[N], b[N], c[N];
  unsigned char d[N], e[N];
  for (int i = 0; i < N; ++i)
    {
      d[i] = i * 2 + 3;
      e[i] = i + 100;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c, d, e);
  for (int i = 0; i < N; ++i)
    if (a[i] != i * 2 + 3
	|| b[i] != i + 100
	|| c[i] != i * 3 + 103)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not {vect_recog_over_widening_pattern: detected} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
