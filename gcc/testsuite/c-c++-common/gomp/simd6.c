/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

extern int a[1024];

struct S { int i; } s;

void
f1 (int x, float f, int *p)
{
  int i;
  #pragma omp simd aligned(x : 32)	/* { dg-error "neither a pointer nor an array" } */
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(f)		/* { dg-error "neither a pointer nor an array" } */
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(s : 16)	/* { dg-error "neither a pointer nor an array" } */
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(a : 8)
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(p : 8)
  for (i = 0; i < 1024; i++)
    a[i]++;
}
