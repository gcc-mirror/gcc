/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -Ofast -fdump-tree-vect-details" } */
/* { dg-final { scan-assembler-not "kshift" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 64 byte vectors" 6 "vect" { target { ! ia32 } } } } */

void
foo (float* a, float* b, int* c, int* d, long long* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      long long tmp = c[i];
      long long tmp2 = d[i];
      if (a[i] < b[i])
	tmp = tmp2;
      e[i] = tmp;
    }
}

void
foo1 (double* a, double* b, long long* c, long long* d, int* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      int tmp = (int)c[i];
      int tmp2 = (int)d[i];
      if (a[i] < b[i])
	tmp = tmp2;
      e[i] = tmp;
    }
}

void
foo2 (float* a, float* b, int* c, int* d, double* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      double tmp = c[i];
      double tmp2 = d[i];
      if (a[i] < b[i])
	tmp = tmp2;
      e[i] = tmp;
    }
}

void
foo3 (double* a, double* b, long long* c, long long* d, float* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      float tmp = c[i];
      float tmp2 = d[i];
      if (a[i] < b[i])
	tmp = tmp2;
      e[i] = tmp;
    }
}

void
foo4 (int* a, int* b, int* c, int* d, double* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      double tmp = c[i];
      double tmp2 = d[i];
      if (a[i] < b[i])
	tmp = tmp2;
      e[i] = tmp;
    }
}

void
foo5 (long long* a, long long* b, long long* c, long long* d, float* __restrict e, int n)
{
  for (int i = 0 ; i != n; i++)
    {
      float tmp = c[i];
      float tmp2 = d[i];
      if (a[i] < b[i])
	tmp = tmp2;
      e[i] = tmp;
    }
}
