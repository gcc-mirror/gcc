/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -mprefer-vector-width=256 -Ofast" } */
/* { dg-final { scan-assembler-not "kmov" } } */

void
foo (double* a, double* __restrict b, double* c, double* d, int n)
{
  for (int i = 0; i != n; i++)
    {
      double tmp = 0.0;
      if (c[i] > d[i])
	tmp = b[i];
      a[i] = tmp;
    }
}

void
foo1 (double* a, double* __restrict b, double* c, double* d, int n)
{
  for (int i = 0; i != n; i++)
    {
      double tmp = 0.0;
      if (c[i] > d[i])
	a[i] = b[i];
    }
}
