/* Make sure that the reverse permute patterns are optimized
   correctly.  */
/* { dg-do run { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z14 -mzarch -fno-unroll-loops -save-temps" } */

/* { dg-final { scan-assembler-times "vpdi\t" 4 } } */
/* { dg-final { scan-assembler-times "verllg\t" 2 } } */

#include <assert.h>

__attribute__((noipa))
void reversel (long long *restrict a, long long *restrict b, int n)
{
  for (int i = 0; i < n; i += 2)
    {
      a[i + 1] = b[i + 0];
      a[i + 0] = b[i + 1];
    }
}

__attribute__((noipa))
void reversed (double *restrict a, double *restrict b, int n)
{
  for (int i = 0; i < n; i += 2)
    {
      a[i + 1] = b[i + 0];
      a[i + 0] = b[i + 1];
    }
}

__attribute__((noipa))
void reversei (unsigned int *restrict a, unsigned int *restrict b, int n)
{
  for (int i = 0; i < n; i += 4)
    {
      a[i + 3] = b[i + 0];
      a[i + 2] = b[i + 1];
      a[i + 1] = b[i + 2];
      a[i + 0] = b[i + 3];
    }
}

__attribute__((noipa))
void reversef (float *restrict a, float *restrict b, int n)
{
  for (int i = 0; i < n; i += 4)
    {
      a[i + 3] = b[i + 0];
      a[i + 2] = b[i + 1];
      a[i + 1] = b[i + 2];
      a[i + 0] = b[i + 3];
    }
}

int main()
{
  const int n = 1024;
  unsigned int u[n], u2[n];
  long long l[n], l2[n];
  double d[n], d2[n];
  float f[n], f2[n];

  for (int i = 0; i < n; i++)
    {
      u[i] = i;
      l[i] = i;
      d[i] = i;
      f[i] = i;
      u2[i] = i;
      l2[i] = i;
      d2[i] = i;
      f2[i] = i;
    }

  reversei (u2, u, n);
  reversel (l2, l, n);
  reversed (d2, d, n);
  reversef (f2, f, n);

  for (int i = 0; i < n - 16; i++)
    {
      assert (u[i] == u2[i / (16 / sizeof (u[0])) * (16 / sizeof (u[0])) + 16 / sizeof (u[0]) - 1 - i % (16 / sizeof (u[0]))]);
      assert (l[i] == l2[i / (16 / sizeof (l[0])) * (16 / sizeof (l[0])) + 16 / sizeof (l[0]) - 1 - i % (16 / sizeof (l[0]))]);
      assert (d[i] == d2[i / (16 / sizeof (d[0])) * (16 / sizeof (d[0])) + 16 / sizeof (d[0]) - 1 - i % (16 / sizeof (d[0]))]);
      assert (f[i] == f2[i / (16 / sizeof (f[0])) * (16 / sizeof (f[0])) + 16 / sizeof (f[0]) - 1 - i % (16 / sizeof (f[0]))]);
    }
}
