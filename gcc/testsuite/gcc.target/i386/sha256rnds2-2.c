/* { dg-do run } */
/* { dg-options "-O2 -msha" } */
/* { dg-require-effective-target sha } */

#include "sha-check.h"
#include "m128-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static int
ch (int e, int f, int g)
{
  return (e & f) ^ (~e & g);
}

static int
maj (int a, int b, int c)
{
  return (a & b) ^ (a & c) ^ (b & c);
}

static int
s0 (int a)
{
  return __rord (a, 2) ^ __rord (a, 13) ^ __rord (a, 22);
}

static int
s1 (int e)
{
  return __rord (e, 6) ^ __rord (e, 11) ^ __rord (e, 25);
}

static void
compute_sha256rnds2 (int *src0, int *src1, int *src2, int *res)
{
  int wk[2] = { src0[0], src0[1] };
  int a[3], b[3], c[3], d[3], e[3], f[3], g[3], h[3];

  a[0] = src2[3];
  b[0] = src2[2];
  c[0] = src1[3];
  d[0] = src1[2];
  e[0] = src2[1];
  f[0] = src2[0];
  g[0] = src1[1];
  h[0] = src1[0];

  int i;
  for (i = 0; i <= 1; i++)
    {
      a[i+1] = ch (e[i], f[i], g[i]) + s1 (e[i]) + wk[i] + h[i]
	       + maj (a[i], b[i], c[i]) + s0 (a[i]);
      b[i+1] = a[i];
      c[i+1] = b[i];
      d[i+1] = c[i];
      e[i+1] = ch (e[i], f[i], g[i]) + s1 (e[i]) + wk[i] + h[i] + d[i];
      f[i+1] = e[i];
      g[i+1] = f[i];
      h[i+1] = g[i];
    }

  res[0] = f[2];
  res[1] = e[2];
  res[2] = b[2];
  res[3] = a[2];
}

static void
sha_test (void)
{
  union128i_d s0, s1, s2, res;
  int res_ref[4];

  s0.x = _mm_set_epi32 (0, 0, 111, 222);
  s1.x = _mm_set_epi32 (333, 444, 555, 666);
  s2.x = _mm_set_epi32 (777, 888, 999, 123);

  res.x = _mm_sha256rnds2_epu32 (s1.x, s2.x, s0.x);

  compute_sha256rnds2 (s0.a, s1.a, s2.a, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
