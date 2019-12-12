/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fopenmp-simd -O2 -mavx512f -fdump-ipa-icf-optimized" } */

#define N 1024
int a[N];

void
test_simdlen1 (void)
{
  int i;
  #pragma omp simd simdlen (4)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

void
test_simdlen2 (void)
{
  int i;
  #pragma omp simd simdlen (8)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

void
test_safelen1 (void)
{
  int i;
  #pragma omp simd safelen (4)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

void
test_safelen2 (void)
{
  int i;
  #pragma omp simd safelen (8)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

int d[1024];

int
test_simduid1 (int j, int b)
{
  int l, c = 0;
#pragma omp simd reduction(+: c)
  for (l = 0; l < b; ++l)
    c += d[j + l];
  return c;
}

int
test_simduid2 (int j, int b)
{
  int l, c2 = 0;
#pragma omp simd reduction(+: c2)
  for (l = 0; l < b; ++l)
    c2 += d[j + l];
  return c2;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:test_simduid1->test_simduid2" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
