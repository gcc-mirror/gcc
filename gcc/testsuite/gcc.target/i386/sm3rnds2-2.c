/* { dg-do run } */
/* { dg-options "-O2 -msm3" } */
/* { dg-require-effective-target sm3 } */

#include "sm3-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static unsigned
p0 (unsigned w)
{
  return (w ^ rol32 (w, 9) ^ rol32 (w, 17));
}

static unsigned
ff (unsigned x, unsigned y, unsigned z, int round)
{
  if (round < 16)
    return (x ^ y ^ z);
  else
    return ((x & y) | (x & z) | (y & z));
}

static unsigned
gg (unsigned x, unsigned y, unsigned z, int round)
{
  if (round < 16)
    return (x ^ y ^ z);
  else
    return ((x & y) | ((~x) & z));
}

static void
compute_sm3rnds2 (int *src0, int *src1, int *src2, int imm, int *res)
{
  unsigned s1, s2, t1, t2, co;
  unsigned w[6], a[3], b[3], c[3], d[3], e[3], f[3], g[3], h[3];
  int round, i;

  a[0] = src1[3];
  b[0] = src1[2];
  c[0] = src0[3];
  d[0] = src0[2];
  e[0] = src1[1];
  f[0] = src1[0];
  g[0] = src0[1];
  h[0] = src0[0];
  w[0] = src2[0];
  w[1] = src2[1];
  w[4] = src2[2];
  w[5] = src2[3];

  c[0] = rol32 (c[0], 9);
  d[0] = rol32 (d[0], 9);
  g[0] = rol32 (g[0], 19);
  h[0] = rol32 (h[0], 19);

  round = imm & 0x3e;
  if (round < 16)
    co = 0x79cc4519;
  else
    co = 0x7a879d8a;
  co = rol32 (co, round);

  for (i = 0; i < 2; i++)
  {
    s1 = rol32 ((rol32 (a[i], 12) + e[i] + co), 7);
    s2 = s1 ^ rol32 (a[i], 12);
    t1 = ff (a[i], b[i], c[i], round) + d[i] + s2 + (w[i] ^ w[i + 4]);
    t2 = gg (e[i], f[i], g[i], round) + h[i] + s1 + w[i];
    d[i + 1] = c[i];
    c[i + 1] = rol32 (b[i], 9);
    b[i + 1] = a[i];
    a[i + 1] = t1;
    h[i + 1] = g[i];
    g[i + 1] = rol32 (f[i], 19);
    f[i + 1] = e[i];
    e[i + 1] = p0 (t2);
    co = rol32 (co, 1);
  }

  res[3] = a[2];
  res[2] = b[2];
  res[1] = e[2];
  res[0] = f[2];
}

static void
sm3_test (void)
{
  union128i_d s1, s2, s3, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (555, 666, 777, 888);
  s3.x = _mm_set_epi32 (999, 123, 456, 789);

  res.x = _mm_sm3rnds2_epi32 (s1.x, s2.x, s3.x, 22);

  compute_sm3rnds2 (s1.a, s2.a, s3.a, 22, res_ref);

  if (check_union128i_d (res, res_ref))
    abort ();
}
