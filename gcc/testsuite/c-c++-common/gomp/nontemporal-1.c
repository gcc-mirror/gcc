/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#define N 1024
int a[N], b[N], c[N], d[N];

void
foo (void)
{
  int i;
  #pragma omp simd nontemporal (a, b)
  for (i = 0; i < N; ++i)
    a[i] = b[i] + c[i];
  #pragma omp simd nontemporal (d)
  for (i = 0; i < N; ++i)
    d[i] = 2 * c[i];
}
