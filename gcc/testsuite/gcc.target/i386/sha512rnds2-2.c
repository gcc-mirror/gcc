/* { dg-do run } */
/* { dg-options "-O2 -msha512" } */
/* { dg-require-effective-target sha512 } */

#include "sha512-check.h"
#include <emmintrin.h>
#include <immintrin.h>

static unsigned long long
ch (unsigned long long e, unsigned long long f, unsigned long long g)
{
  return (e & f) ^ (~e & g);
}

static unsigned long long
maj (unsigned long long a, unsigned long long b, unsigned long long c)
{
  return (a & b) ^ (a & c) ^ (b & c);
}

static unsigned long long
s0 (unsigned long long w)
{
  return ror64 (w, 28) ^ ror64 (w, 34) ^ ror64 (w, 39);
}

static unsigned long long
s1 (unsigned long long w)
{
  return ror64 (w, 14) ^ ror64 (w, 18) ^ ror64 (w, 41);
}

static void
compute_sha512rnds2(long long* src0, long long* src1, long long* src2, long long* res)
{
  unsigned long long wk[2] = { src2[0], src2[1] };
  unsigned long long a[3], b[3], c[3], d[3], e[3], f[3], g[3], h[3];

  a[0] = src1[3];
  b[0] = src1[2];
  c[0] = src0[3];
  d[0] = src0[2];
  e[0] = src1[1];
  f[0] = src1[0];
  g[0] = src0[1];
  h[0] = src0[0];

  int i;
  for (i = 0; i <= 1; i++)
  {
    a[i + 1] = ch (e[i], f[i], g[i]) + s1 (e[i]) + wk[i] + h[i]
      + maj (a[i], b[i], c[i]) + s0 (a[i]);
    b[i + 1] = a[i];
    c[i + 1] = b[i];
    d[i + 1] = c[i];
    e[i + 1] = ch (e[i], f[i], g[i]) + s1 (e[i]) + wk[i] + h[i] + d[i];
    f[i + 1] = e[i];
    g[i + 1] = f[i];
    h[i + 1] = g[i];
  }

  res[0] = f[2];
  res[1] = e[2];
  res[2] = b[2];
  res[3] = a[2];
}

static void
sha512_test (void)
{
  union256i_q s0, s1, res;
  union128i_q s2;
  long long res_ref[4];

  s0.x = _mm256_set_epi64x (111, 222, 333, 444);
  s1.x = _mm256_set_epi64x (555, 666, 777, 888);
  s2.x = _mm_set_epi64x (999, 123);

  res.x = _mm256_sha512rnds2_epi64 (s0.x, s1.x, s2.x);

  compute_sha512rnds2 (s0.a, s1.a, s2.a, res_ref);

  if (check_union256i_q (res, res_ref))
    abort();
}
