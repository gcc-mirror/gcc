// { dg-do compile }
// { dg-options "-fopenmp" }

extern int a[1024];
int (&b)[1024] = a;

struct S { int s; } s, &t = s;

void
f1 (int &x, float &f, int *&p)
{
  int i;
  #pragma omp simd aligned(x : 32)	// { dg-error "neither a pointer nor an array" }
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(f)		// { dg-error "neither a pointer nor an array" }
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(t : 16)	// { dg-error "neither a pointer nor an array" }
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(a : 8)
  for (i = 0; i < 1024; i++)
    a[i]++;
  #pragma omp simd aligned(b : 8)
  for (i = 0; i < 1024; i++)
    b[i]++;
  #pragma omp simd aligned(p : 8)
  for (i = 0; i < 1024; i++)
    a[i]++;
}
