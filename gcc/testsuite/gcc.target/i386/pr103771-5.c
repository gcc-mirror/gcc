/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O3 -fno-trapping-math -fdump-tree-vect-details" } */
/* { dg-final { scan-assembler-not "kshift" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 64 byte vectors" 4 "vect" { target { ! ia32 } } } } */

void
foo (float* a, float* b, float* c, float* d, double* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      float tmp = c[i] + d[i];
      if (a[i] < b[i])
	tmp = 0.0;
      e[i] = tmp;
    }
}

void
foo1 (int* a, int* b, float* c, float* d, double* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      float tmp = c[i] + d[i];
      if (a[i] < b[i])
	tmp = 0.0;
      e[i] = tmp;
    }
}


void
foo2 (double* a, double* b, double* c, double* d, float* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      float tmp = c[i] + d[i];
      if (a[i] < b[i])
	tmp = 0.0;
      e[i] = tmp;
    }
}

void
foo3 (long long* a, long long* b, double* c, double* d, float* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      float tmp = c[i] + d[i];
      if (a[i] < b[i])
	tmp = 0.0;
      e[i] = tmp;
    }
}

