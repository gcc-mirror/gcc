/* { dg-do compile } */
/* { dg-options "-fopenmp" } */
/* { dg-additional-options "-std=c99" { target c } } */

extern int a[1024], b[1024], k, l, m;

void
foo ()
{
  int i;
  #pragma omp simd safelen(16) aligned(a, b : 32)
  for (i = 0; i < 1024; i++)
    a[i] *= b[i];
}

void
bar (int *p)
{
  int i;
  #pragma omp simd safelen(16) aligned(a, p : 32) linear(k, l : m + 1)
  for (i = 0; i < 1024; i++)
    a[i] *= p[i], k += m + 1;
}

void
baz (int *p)
{
  #pragma omp simd safelen(16) aligned(a, p : 32) linear(k, l : m + 1)
  for (int i = 0; i < 1024; i++)
    a[i] *= p[i], k += m + 1;
}
