/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O3 -mavx2 -fno-common -mtune=skylake" } */

#include "avx2-check.h"

#define N 1024
float vf1[N+16], vf2[N], vf3[N];
int k[N];

__attribute__((noinline, noclone)) void
foo (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      float f;
      if (vf3[i] < 0.0f)
	f = vf1[k[i]];
      else
	f = 7.0f;
      vf2[i] = f;
    }
}

static void
avx2_test (void)
{
  int i;
  for (i = 0; i < N + 16; i++)
    {
      vf1[i] = 5.5f * i;
      if (i >= N)
	continue;
      vf2[i] = 2.0f;
      vf3[i] = (i & 1) ? i : -i - 1;
      k[i] = (i & 1) ? ((i & 2) ? -i : N / 2 + i) : (i * 7) % N;
      asm ("");
    }
  foo ();
  for (i = 0; i < N; i++)
    if (vf1[i] != 5.5 * i
	|| vf2[i] != ((i & 1) ? 7.0f : 5.5f * ((i * 7) % N))
	|| vf3[i] != ((i & 1) ? i : -i - 1)
	|| k[i] != ((i & 1) ? ((i & 2) ? -i : N / 2 + i) : ((i * 7) % N)))
      abort ();
}
