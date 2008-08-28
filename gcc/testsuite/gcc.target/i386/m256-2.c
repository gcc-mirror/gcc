/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-mavx" } */

#include <assert.h>
#include "avx-check.h"

struct m128
{
  __m128 v;
};

struct m256d
{
  __m256d v;
};

struct m128 n1 = { { -283.3, -23.3, 213.4, 1119.03 } };
struct m256d n2 = { { -93.83, 893.318, 3884.34, -3134.3 } };
__m256i n3 = { 893, -3180, 3334, -3984 };
int n4 = -30;
double n5 = 40.3;
__m128i n6 = { 8931, -13984 };
__m128d n7 = { 1893.318, -31134.3 };
__m256 n8 =
{
  -913.87, 8193.518, 312884.34, -9134.9,
  -19093.83, 89312.318, 7884.84, -4134.3
};
__m128 n9 = { -1283.3, -213.3, 3213.4, 81119.03 };
__m128i n10 = { 28131, -313684 };
int n11 = 103;
double n12 = -3004.3;
struct m256d n13 =  { { 913.73, -93.38, 84.34, -734.3 } };
__m128d n14 = { -73.378, 934.31 };
__m256 n15 =
{
  13.73, -8193.318, 384.74, 734.9,
  193.83, 312.78, 7884.34, -8134.3
};
__m128i n16 = { 831, -3849 };

void
__attribute__((noinline))
m256_test (struct m128 a1, struct m256d a2, __m256i a3, int a4, double a5,
	   __m128i a6, __m128d a7, __m256 a8, __m128 a9, __m128i a10,
	   int a11, double a12, struct m256d a13, __m128d a14, __m256 a15,
	   __m128i a16)
{
  assert (__builtin_memcmp (&a1, &n1, sizeof (a1)) == 0);
  assert (__builtin_memcmp (&a2, &n2, sizeof (a2)) == 0);
  assert (__builtin_memcmp (&a3, &n3, sizeof (a3)) == 0);
  assert (a4 == n4);
  assert (a5 == n5);
  assert (__builtin_memcmp (&a6, &n6, sizeof (a6)) == 0);
  assert (__builtin_memcmp (&a7, &n7, sizeof (a7)) == 0);
  assert (__builtin_memcmp (&a8, &n8, sizeof (a8)) == 0);
  assert (__builtin_memcmp (&a9, &n9, sizeof (a9)) == 0);
  assert (__builtin_memcmp (&a10, &n10, sizeof (a10)) == 0);
  assert (a11 == n11);
  assert (a12 == n12);
  assert (__builtin_memcmp (&a13, &n13, sizeof (a13)) == 0);
  assert (__builtin_memcmp (&a14, &n14, sizeof (a14)) == 0);
  assert (__builtin_memcmp (&a15, &n15, sizeof (a15)) == 0);
  assert (__builtin_memcmp (&a16, &n16, sizeof (a16)) == 0);
}

static void
avx_test (void)
{
  m256_test (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12,
	     n13, n14, n15, n16);
} 
