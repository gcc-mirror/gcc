/* { dg-do compile } */

#define N 1024
extern int a[N], b[N], c[N], d[N];

void
foo (void)
{
  int i;
  #pragma omp simd nontemporal (a, b) aligned (a, b, c)
  for (i = 0; i < N; ++i)
    a[i] = b[i] + c[i];
  #pragma omp simd nontemporal (d) nontemporal (d)	/* { dg-error "'d' appears more than once in 'nontemporal' clauses" } */
  for (i = 0; i < N; ++i)
    d[i] = 2 * c[i];
  #pragma omp simd nontemporal (a, b, b)		/* { dg-error "'b' appears more than once in 'nontemporal' clauses" } */
  for (i = 0; i < N; ++i)
    a[i] += b[i] + c[i];
}
