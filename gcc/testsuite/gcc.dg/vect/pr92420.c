/* { dg-additional-options "-mavx2" { target avx_runtime } } */

#include "tree-vect.h"

#define N 16
struct C { int r, i; };
struct C a[N], b[N], c[N], d[N], e[N];

__attribute__((noipa)) static void
foo (struct C *__restrict x, struct C *__restrict y, struct C *__restrict z, int w)
{
  int i;
  for (int i = 0; i < w; i++)
    {
      z[i].r = x[i].r * y[-1 - i].r - x[i].i * y[-1 - i].i;
      z[i].i = x[i].i * y[-1 - i].r + x[i].r * y[-1 - i].i;
    }
}

__attribute__((noipa)) static void
bar (struct C *__restrict x, struct C *__restrict y, struct C *__restrict z, int w)
{
  int i;
  for (int i = 0; i < w; i++)
    {
      z[i].r = x[i].r * y[i].r - x[i].i * y[i].i;
      z[i].i = x[i].i * y[i].r + x[i].r * y[i].i;
    }
}

int
main ()
{
  check_vect ();
  int i;
  for (i = 0; i < N; ++i)
    {
      a[i].r = N - i; a[i].i = i - N;
      b[i].r = i - N; b[i].i = i + N;
      c[i].r = -1 - i; c[i].i = 2 * N - 1 - i;
    }
  foo (a, b + N, d, N);
  bar (a, c, e, N);
  for (i = 0; i < N; ++i)
    if (d[i].r != e[i].r || d[i].i != e[i].i)
      __builtin_abort ();
  return 0;
}
