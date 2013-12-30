/* PR tree-optimization/59591 */
/* { dg-do run } */
/* { dg-additional-options "-fopenmp-simd" } */

#ifndef CHECK_H
#include "tree-vect.h"
#endif

extern void abort (void);

long long int p[256], r[256], t[256];
int q[256];

__attribute__((noinline, noclone)) void
foo (void)
{
  int i;
  #pragma omp simd safelen(64)
  for (i = 0; i < 256; i++)
    if (r[i] > 32LL)
      t[i] = p[q[i]];
}

__attribute__((noinline, noclone)) void
bar (void)
{
  int i;
  for (i = 0; i < 256; i++)
    {
      r[i] = ((i >> 2) & (1 << (i & 3))) ? 32 + i : 32 - i;
      q[i] = r[i] > 32 ? ((i * 7) % 256) : 258 + i;
      p[i] = i * 11;
      t[i] = i * 13;
    }
  foo ();
  for (i = 0; i < 256; i++)
    if ((i >> 2) & (1 << (i & 3)))
      {
	if (t[i] != ((i * 7) % 256) * 11)
	  abort ();
      }
    else if (t[i] != i * 13)
      abort ();
}

#ifndef CHECK_H
int
main ()
{
  check_vect ();
  bar ();
  return 0;
}
#endif

/* { dg-final { cleanup-tree-dump "vect" } } */
