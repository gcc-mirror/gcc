/* PR target/85918 */
/* { dg-do run } */
/* { dg-require-effective-target avx512dq } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O3 -mavx512dq -mavx512vl -mprefer-vector-width=512 -fno-vect-cost-model" } */

#define AVX512DQ
#define AVX512VL
#define DO_TEST avx512dqvl_test

static void avx512dqvl_test (void);

#include "avx512-check.h"

#define N 16

long long ll[N] __attribute__((aligned (64)));
unsigned long long ull[N] __attribute__((aligned (64)));
float f[N] __attribute__((aligned (64)));
double d[N] __attribute__((aligned (64)));

__attribute__((noipa)) void
ll2d1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    d[i] = ll[i];
}

__attribute__((noipa)) void
ull2d1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    d[i] = ull[i];
}

__attribute__((noipa)) void
d2ll1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ll[i] = d[i];
}

__attribute__((noipa)) void
d2ull1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ull[i] = d[i];
}

__attribute__((noipa)) void
ll2f1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    f[i] = ll[i];
}

__attribute__((noipa)) void
ull2f1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    f[i] = ull[i];
}

__attribute__((noipa)) void
f2ll1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ll[i] = f[i];
}

__attribute__((noipa)) void
f2ull1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ull[i] = f[i];
}

__attribute__((noipa)) void
ll2d2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    d[i] = ll[i];
}

__attribute__((noipa)) void
ull2d2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    d[i] = ull[i];
}

__attribute__((noipa)) void
d2ll2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ll[i] = d[i];
}

__attribute__((noipa)) void
d2ull2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ull[i] = d[i];
}

__attribute__((noipa)) void
ll2f2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    f[i] = ll[i];
}

__attribute__((noipa)) void
ull2f2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    f[i] = ull[i];
}

__attribute__((noipa)) void
f2ll2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ll[i] = f[i];
}

__attribute__((noipa)) void
f2ull2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ull[i] = f[i];
}

__attribute__((noipa)) void
ll2d3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    d[i] = ll[i];
}

__attribute__((noipa)) void
ull2d3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    d[i] = ull[i];
}

__attribute__((noipa)) void
d2ll3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ll[i] = d[i];
}

__attribute__((noipa)) void
d2ull3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ull[i] = d[i];
}

__attribute__((noipa)) void
ll2f3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    f[i] = ll[i];
}

__attribute__((noipa)) void
ull2f3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    f[i] = ull[i];
}

__attribute__((noipa)) void
f2ll3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ll[i] = f[i];
}

__attribute__((noipa)) void
f2ull3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ull[i] = f[i];
}

unsigned long long ullt[] = {
  13835058055282163712ULL, 9223653511831486464ULL, 9218868437227405312ULL,
  1ULL, 9305281255077576704ULL, 1191936ULL, 18446462598732840960ULL, 0ULL,
  9223372036854775808ULL, 4611686018427387904ULL, 2305843009213693952ULL,
  9ULL, 9223653511831486464ULL, 0ULL, 65536ULL, 131071ULL
};
float uft[] = {
  13835058055282163712.0f, 9223653511831486464.0f, 9218868437227405312.0f,
  1.0f, 9305281255077576704.0f, 1191936.0f, 18446462598732840960.0f, 0.0f,
  9223372036854775808.0f, 4611686018427387904.0f, 2305843009213693952.0f,
  9.0f, 9223653511831486464.0f, 0.0f, 65536.0f, 131071.0f
};
long long llt[] = {
  9223090561878065152LL, -9223372036854775807LL - 1, -9223090561878065152LL,
  -4LL, -8074672656898588672LL, 8074672656898588672LL, 29LL, -15LL,
  7574773098260463616LL, -7579276697887834112LL, -8615667562136469504LL,
  148LL, -255LL, 9151595917793558528LL, -9218868437227405312LL, 9LL
};
float ft[] = {
  9223090561878065152.0f, -9223372036854775808.0f, -9223090561878065152.0f,
  -4.0f, -8074672656898588672.0f, 8074672656898588672.0f, 29.0f, -15.0f,
  7574773098260463616.0f, -7579276697887834112.0f, -8615667562136469504.0f,
  148.0f, -255.0f, 9151595917793558528.0f, -9218868437227405312.0f, 9.0f
};

static void
avx512dqvl_test (void)
{
  int i;
  for (i = 0; i < 4; i++)
    {
      ll[i] = llt[i];
      ull[i] = ullt[i];
    }
  ll2d1 ();
  for (i = 0; i < 4; i++)
    if (d[i] != ft[i])
      abort ();
  ull2d1 ();
  for (i = 0; i < 4; i++)
    if (d[i] != uft[i])
      abort ();
    else
      d[i] = ft[i + 4];
  d2ll1 ();
  for (i = 0; i < 4; i++)
    if (ll[i] != llt[i + 4])
      abort ();
    else
      d[i] = uft[i + 4];
  d2ull1 ();
  for (i = 0; i < 4; i++)
    if (ull[i] != ullt[i + 4])
      abort ();
    else
      {
        ll[i] = llt[i + 8];
	ull[i] = ullt[i + 8];
      }
  ll2f1 ();
  for (i = 0; i < 4; i++)
    if (f[i] != ft[i + 8])
      abort ();
  ull2f1 ();
  for (i = 0; i < 4; i++)
    if (f[i] != uft[i + 8])
      abort ();
    else
      f[i] = ft[i + 12];
  f2ll1 ();
  for (i = 0; i < 4; i++)
    if (ll[i] != llt[i + 12])
      abort ();
    else
      f[i] = uft[i + 12];
  f2ull1 ();
  for (i = 0; i < 4; i++)
    if (ull[i] != ullt[i + 12])
      abort ();
  for (i = 0; i < 8; i++)
    {
      ll[i] = llt[i];
      ull[i] = ullt[i];
    }
  ll2d2 ();
  for (i = 0; i < 8; i++)
    if (d[i] != ft[i])
      abort ();
  ull2d2 ();
  for (i = 0; i < 8; i++)
    if (d[i] != uft[i])
      abort ();
    else
      {
        d[i] = ft[i];
        ll[i] = 1234567LL;
        ull[i] = 7654321ULL;
      }
  d2ll2 ();
  for (i = 0; i < 8; i++)
    if (ll[i] != llt[i])
      abort ();
    else
      d[i] = uft[i];
  d2ull2 ();
  for (i = 0; i < 8; i++)
    if (ull[i] != ullt[i])
      abort ();
    else
      {
        ll[i] = llt[i + 8];
	ull[i] = ullt[i + 8];
      }
  ll2f2 ();
  for (i = 0; i < 8; i++)
    if (f[i] != ft[i + 8])
      abort ();
  ull2f2 ();
  for (i = 0; i < 8; i++)
    if (f[i] != uft[i + 8])
      abort ();
    else
      {
	f[i] = ft[i + 8];
	ll[i] = 1234567LL;
	ull[i] = 7654321ULL;
      }
  f2ll2 ();
  for (i = 0; i < 8; i++)
    if (ll[i] != llt[i + 8])
      abort ();
    else
      f[i] = uft[i + 8];
  f2ull2 ();
  for (i = 0; i < 8; i++)
    if (ull[i] != ullt[i + 8])
      abort ();
  for (i = 0; i < 16; i++)
    {
      ll[i] = llt[i];
      ull[i] = ullt[i];
    }
  ll2d3 ();
  for (i = 0; i < 16; i++)
    if (d[i] != ft[i])
      abort ();
  ull2d3 ();
  for (i = 0; i < 16; i++)
    if (d[i] != uft[i])
      abort ();
    else
      {
        d[i] = ft[i];
        ll[i] = 1234567LL;
        ull[i] = 7654321ULL;
      }
  d2ll3 ();
  for (i = 0; i < 16; i++)
    if (ll[i] != llt[i])
      abort ();
    else
      d[i] = uft[i];
  d2ull3 ();
  for (i = 0; i < 16; i++)
    if (ull[i] != ullt[i])
      abort ();
    else
      {
        ll[i] = llt[i];
	ull[i] = ullt[i];
	f[i] = 3.0f;
	d[i] = 4.0;
      }
  ll2f3 ();
  for (i = 0; i < 16; i++)
    if (f[i] != ft[i])
      abort ();
  ull2f3 ();
  for (i = 0; i < 16; i++)
    if (f[i] != uft[i])
      abort ();
    else
      {
	f[i] = ft[i];
	ll[i] = 1234567LL;
	ull[i] = 7654321ULL;
      }
  f2ll3 ();
  for (i = 0; i < 16; i++)
    if (ll[i] != llt[i])
      abort ();
    else
      f[i] = uft[i];
  f2ull3 ();
  for (i = 0; i < 16; i++)
    if (ull[i] != ullt[i])
      abort ();
}
