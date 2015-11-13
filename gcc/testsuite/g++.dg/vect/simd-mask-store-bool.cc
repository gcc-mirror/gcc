/* { dg-do compile } */
/* { dg-require-effective-target vect_condition } */
/* { dg-additional-options "-mavx512bw" { target { i?86-*-* x86_64-*-* } } } */

#define N 1024

int a[N], b[N], c[N];
bool d[N];

void
test (void)
{
  int i;
#pragma omp simd safelen(64)
  for (i = 0; i < N; i++)
    if (a[i] > 0)
      d[i] = b[i] > c[i];
}
