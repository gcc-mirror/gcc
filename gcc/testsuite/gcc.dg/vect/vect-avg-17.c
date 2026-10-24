/* { dg-additional-options "-O3 -Wno-psabi" } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 128

int a[N], b[N], c[N], d[N], r[N];

void __attribute__ ((noipa))
floor2 (void)
{
  for (int i = 0; i < N; ++i)
    r[i] = (a[i] + b[i]) >> 1;
}

void __attribute__ ((noipa))
ceil2 (void)
{
  for (int i = 0; i < N; ++i)
    r[i] = (a[i] + b[i] + 1) >> 1;
}

void __attribute__ ((noipa))
ceil4 (void)
{
  for (int i = 0; i < N; ++i)
    r[i] = (a[i] + b[i] + c[i] + d[i] + 1) >> 1;
}

typedef short v8hi __attribute__ ((vector_size (16)));

v8hi __attribute__ ((noipa))
floor_v8hi (v8hi x, v8hi y)
{
  return (x + y) >> 1;
}

v8hi __attribute__ ((noipa))
ceil_v8hi (v8hi x, v8hi y)
{
  return (x + y + 1) >> 1;
}

int
main (void)
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      a[i] = (i * 37 % 101 - 50) * 1000003;
      b[i] = (i * 53 % 97 - 48) * 999983 + (i & 1);
      c[i] = -(i * 29 % 89) * 1000033 - 1;
      d[i] = (i * 17 % 83 - 41) * 999979;
    }

  floor2 ();
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (r[i] != (a[i] + b[i]) >> 1)
      __builtin_abort ();

  ceil2 ();
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (r[i] != (a[i] + b[i] + 1) >> 1)
      __builtin_abort ();

  ceil4 ();
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (r[i] != (a[i] + b[i] + c[i] + d[i] + 1) >> 1)
      __builtin_abort ();

  v8hi x = { -32767, -3, -3, 5, 32766, 0, -1, 7 };
  v8hi y = { -1, -2, 4, 6, -32767, -1, -1, -8 };
  v8hi f = floor_v8hi (x, y);
  v8hi g = ceil_v8hi (x, y);
#pragma GCC novector
  for (int i = 0; i < 8; ++i)
    if (f[i] != (x[i] + y[i]) >> 1 || g[i] != (x[i] + y[i] + 1) >> 1)
      __builtin_abort ();

  return 0;
}
