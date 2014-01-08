/* { dg-do run } */
/* { dg-options "-O2 -msha" } */
/* { dg-require-effective-target sha } */

#include "sha-check.h"
#include "m128-check.h"
#include <x86intrin.h>
#include <immintrin.h>

static int
f0 (int b, int c, int d)
{
  return (b & c) ^ (~b & d);
}

static int
f1 (int b, int c, int d)
{
  return b ^ c ^ d;
}

static int
f2 (int b, int c, int d)
{
  return (b & c) ^ (b & d) ^ (c & d);
}

int (*f_arr[4])(int, int, int) = { f0, f1, f2, f1 };
const int k_arr[4] = { 0x5A827999, 0x6ED9EBA1, 0x8F1BBCDC, 0xCA62C1D6 };


static void
compute_sha1rnds4 (int *src1, int *src2, int imm, int *res)
{
  int k = k_arr[imm];
  int (*f)(int, int, int) = f_arr[imm];

  int w[4] = { src2[3], src2[2], src2[1], src2[0] };
  int a[5], b[5], c[5], d[5], e[5];

  a[0] = src1[3];
  b[0] = src1[2];
  c[0] = src1[1];
  d[0] = src1[0];
  e[0] = 0;

  int i;
  for (i = 0; i <= 3; i++)
    {
      a[i+1] = f(b[i], c[i], d[i]) + __rold (a[i], 5) + w[i] + e[i] + k;
      b[i+1] = a[i];
      c[i+1] = __rold (b[i], 30);
      d[i+1] = c[i];
      e[i+1] = d[i];
    }

  res[0] = d[4];
  res[1] = c[4];
  res[2] = b[4];
  res[3] = a[4];
}


static void
sha_test (void)
{
  int imm;
  union128i_d s1, s2, res;
  int res_ref[4];

  s1.x = _mm_set_epi32 (111, 222, 333, 444);
  s2.x = _mm_set_epi32 (555, 666, 777, 888);

  res.x = _mm_sha1rnds4_epu32 (s1.x, s2.x, 0);
  compute_sha1rnds4 (s1.a, s2.a, 0, res_ref);
  if (check_union128i_d (res, res_ref))
    abort ();

  res.x = _mm_sha1rnds4_epu32 (s1.x, s2.x, 1);
  compute_sha1rnds4 (s1.a, s2.a, 1, res_ref);
  if (check_union128i_d (res, res_ref))
    abort ();

  res.x = _mm_sha1rnds4_epu32 (s1.x, s2.x, 2);
  compute_sha1rnds4 (s1.a, s2.a, 2, res_ref);
  if (check_union128i_d (res, res_ref))
    abort ();

  res.x = _mm_sha1rnds4_epu32 (s1.x, s2.x, 3);
  compute_sha1rnds4 (s1.a, s2.a, 3, res_ref);
  if (check_union128i_d (res, res_ref))
    abort ();
}
