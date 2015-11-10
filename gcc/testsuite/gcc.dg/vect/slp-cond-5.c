/* { dg-require-effective-target vect_condition } */

#include "tree-vect.h"

#define N 128

static inline int
foo (int x, int y, int a, int b)
{
  if (x >= y && a > b)
    return a;
  else
    return b;
}

__attribute__((noinline, noclone)) void
bar (int * __restrict__ a, int * __restrict__ b,
     int * __restrict__ c, int * __restrict__ d,
     int * __restrict__ e, int w)
{
  int i;
  for (i = 0; i < N/16; i++, a += 16, b += 16, c += 16, d += 16, e += 16)
    {
      e[0] = foo (c[0], d[0], a[0] * w, b[0] * w);
      e[1] = foo (c[1], d[1], a[1] * w, b[1] * w);
      e[2] = foo (c[2], d[2], a[2] * w, b[2] * w);
      e[3] = foo (c[3], d[3], a[3] * w, b[3] * w);
      e[4] = foo (c[4], d[4], a[4] * w, b[4] * w);
      e[5] = foo (c[5], d[5], a[5] * w, b[5] * w);
      e[6] = foo (c[6], d[6], a[6] * w, b[6] * w);
      e[7] = foo (c[7], d[7], a[7] * w, b[7] * w);
      e[8] = foo (c[8], d[8], a[8] * w, b[8] * w);
      e[9] = foo (c[9], d[9], a[9] * w, b[9] * w);
      e[10] = foo (c[10], d[10], a[10] * w, b[10] * w);
      e[11] = foo (c[11], d[11], a[11] * w, b[11] * w);
      e[12] = foo (c[12], d[12], a[12] * w, b[12] * w);
      e[13] = foo (c[13], d[13], a[13] * w, b[13] * w);
      e[14] = foo (c[14], d[14], a[14] * w, b[14] * w);
      e[15] = foo (c[15], d[15], a[15] * w, b[15] * w);
    }
}


int a[N], b[N], c[N], d[N], e[N];

int main ()
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = 5;
      e[i] = 0;

      switch (i % 9)
        {
        case 0: asm (""); c[i] = i; d[i] = i + 1; break;
        case 1: c[i] = 0; d[i] = 0; break;
        case 2: c[i] = i + 1; d[i] = i - 1; break;
        case 3: c[i] = i; d[i] = i + 7; break;
        case 4: c[i] = i; d[i] = i; break;
        case 5: c[i] = i + 16; d[i] = i + 3; break;
        case 6: c[i] = i - 5; d[i] = i; break;
        case 7: c[i] = i; d[i] = i; break;
        case 8: c[i] = i; d[i] = i - 7; break;
        }
    }

  bar (a, b, c, d, e, 2);
  for (i = 0; i < N; i++)
    if (e[i] != ((i % 3) == 0 || i <= 5 ? 10 : 2 * i))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { i?86-*-* x86_64-*-* } } } } */

