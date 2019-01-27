/* PR target/87214 */
/* { dg-do run { target { avx512vl } } } */
/* { dg-options "-O2 -mavx512vl" } */

#define AVX512VL
#define AVX512F_LEN 512
#define AVX512F_LEN_HALF 256
#include "avx512f-check.h"

typedef long long int v4di __attribute__((vector_size (4 * sizeof (long long int))));
typedef double v4df __attribute__((vector_size (4 * sizeof (double))));
typedef long long int v8di __attribute__((vector_size (8 * sizeof (long long int))));
typedef double v8df __attribute__((vector_size (8 * sizeof (double))));
typedef int v8si __attribute__((vector_size (8 * sizeof (int))));
typedef float v8sf __attribute__((vector_size (8 * sizeof (float))));
typedef int v16si __attribute__((vector_size (16 * sizeof (int))));
typedef float v16sf __attribute__((vector_size (16 * sizeof (float))));

__attribute__((noipa)) void
f1 (v4di *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v4di) { 2, 3, 5, 6 });
}

__attribute__((noipa)) void
f2 (v4df *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v4di) { 1, 2, 6, 7 });
}

__attribute__((noipa)) void
f3 (v8di *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v8di) { 2, 3, 5, 6, 8, 9, 11, 12 });
}

__attribute__((noipa)) void
f4 (v8df *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v8di) { 1, 2, 6, 7, 9, 10, 12, 13 });
}

__attribute__((noipa)) void
f5 (v8si *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v8si) { 2, 3, 4, 5, 9, 10, 11, 12 });
}

__attribute__((noipa)) void
f6 (v8sf *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v8si) { 1, 2, 3, 4, 12, 13, 14, 15 });
}

__attribute__((noipa)) void
f7 (v16si *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v16si) { 0, 1, 2, 3, 1, 2, 3, 4, 16, 17, 18, 19, 25, 26, 27, 28 });
}

__attribute__((noipa)) void
f8 (v16sf *p)
{
  p[0] = __builtin_shuffle (p[1], p[2], (v16si) { 1, 2, 3, 4, 4, 5, 6, 7, 17, 18, 19, 20, 18, 19, 20, 21 });
}

static void
test_256 (void)
{
  v4di a[3] = { { 0, 0, 0, 0 }, { 10, 11, 12, 13 }, { 14, 15, 16, 17 } };
  f1 (a);
  if (a[0][0] != 12 || a[0][1] != 13 || a[0][2] != 15 || a[0][3] != 16)
    __builtin_abort ();
  v4df b[3] = { { 0.0, 0.0, 0.0, 0.0 }, { 10.0, 11.0, 12.0, 13.0 }, { 14.0, 15.0, 16.0, 17.0 } };
  f2 (b);
  if (b[0][0] != 11.0 || b[0][1] != 12.0 || b[0][2] != 16.0 || b[0][3] != 17.0)
    __builtin_abort ();
  v8di c[3] = { { 0, 0, 0, 0, 0, 0, 0, 0 }, { 10, 11, 12, 13, 14, 15, 16, 17 }, { 18, 19, 20, 21, 22, 23, 24, 25 } };
  f3 (c);
  if (c[0][0] != 12 || c[0][1] != 13 || c[0][2] != 15 || c[0][3] != 16
      || c[0][4] != 18 || c[0][5] != 19 || c[0][6] != 21 || c[0][7] != 22)
    __builtin_abort ();
  v8df d[3] = { { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0 },
		{ 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0 } };
  f4 (d);
  if (d[0][0] != 11.0 || d[0][1] != 12.0 || d[0][2] != 16.0 || d[0][3] != 17.0
      || d[0][4] != 19.0 || d[0][5] != 20.0 || d[0][6] != 22.0 || d[0][7] != 23.0)
    __builtin_abort ();
  v8si e[3] = { { 0, 0, 0, 0, 0, 0, 0, 0 }, { 10, 11, 12, 13, 14, 15, 16, 17 }, { 18, 19, 20, 21, 22, 23, 24, 25 } };
  f5 (e);
  if (e[0][0] != 12 || e[0][1] != 13 || e[0][2] != 14 || e[0][3] != 15
      || e[0][4] != 19 || e[0][5] != 20 || e[0][6] != 21 || e[0][7] != 22)
    __builtin_abort ();
  v8sf f[3] = { { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
		{ 10.0f, 11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 17.0f },
		{ 18.0f, 19.0f, 20.0f, 21.0f, 22.0f, 23.0f, 24.0f, 25.0f } };
  f6 (f);
  if (f[0][0] != 11.0f || f[0][1] != 12.0f || f[0][2] != 13.0f || f[0][3] != 14.0f
      || f[0][4] != 22.0f || f[0][5] != 23.0f || f[0][6] != 24.0f || f[0][7] != 25.0f)
    __builtin_abort ();
  v16si g[3] = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		 { 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 },
		 { 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41 } };
  f7 (g);
  if (g[0][0] != 10 || g[0][1] != 11 || g[0][2] != 12 || g[0][3] != 13
      || g[0][4] != 11 || g[0][5] != 12 || g[0][6] != 13 || g[0][7] != 14
      || g[0][8] != 26 || g[0][9] != 27 || g[0][10] != 28 || g[0][11] != 29
      || g[0][12] != 35 || g[0][13] != 36 || g[0][14] != 37 || g[0][15] != 38)
    __builtin_abort ();
  v16sf h[3] = { { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		   0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
		 { 10.0f, 11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 17.0f,
		   18.0f, 19.0f, 20.0f, 21.0f, 22.0f, 23.0f, 24.0f, 25.0f },
		 { 26.0f, 27.0f, 28.0f, 29.0f, 30.0f, 31.0f, 32.0f, 33.0f,
		   34.0f, 35.0f, 36.0f, 37.0f, 38.0f, 39.0f, 40.0f, 41.0f } };
  f8 (h);
  if (h[0][0] != 11.0f || h[0][1] != 12.0f || h[0][2] != 13.0f || h[0][3] != 14.0f
      || h[0][4] != 14.0f || h[0][5] != 15.0f || h[0][6] != 16.0f || h[0][7] != 17.0f
      || h[0][8] != 27.0f || h[0][9] != 28.0f || h[0][10] != 29.0f || h[0][11] != 30.0f
      || h[0][12] != 28.0f || h[0][13] != 29.0f || h[0][14] != 30.0f || h[0][15] != 31.0f)
    __builtin_abort ();
}

static void
test_128 (void)
{
}
