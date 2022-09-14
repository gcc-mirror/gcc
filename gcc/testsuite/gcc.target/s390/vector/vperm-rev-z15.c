/* Make sure that the reverse permute patterns are optimized
   correctly.  */
/* { dg-do run { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z15 -mzarch -fno-unroll-loops -save-temps" } */

/* { dg-final { scan-assembler-times "vsterg\t" 2 } } */
/* { dg-final { scan-assembler-times "vsterf\t" 2 } } */
/* { dg-final { scan-assembler-times "vstbrq\t" 1 } } */
/* { dg-final { scan-assembler-times "vperm\t" 0 } } */

#include <assert.h>

__attribute__((noipa))
void reversec (char *restrict a, char *restrict b, int n)
{
  for (int i = 0; i < n; i += 16)
    {
      a[i + 0] = b[i + 15];
      a[i + 1] = b[i + 14];
      a[i + 2] = b[i + 13];
      a[i + 3] = b[i + 12];
      a[i + 4] = b[i + 11];
      a[i + 5] = b[i + 10];
      a[i + 6] = b[i + 9];
      a[i + 7] = b[i + 8];
      a[i + 8] = b[i + 7];
      a[i + 9] = b[i + 6];
      a[i + 10] = b[i + 5];
      a[i + 11] = b[i + 4];
      a[i + 12] = b[i + 3];
      a[i + 13] = b[i + 2];
      a[i + 14] = b[i + 1];
      a[i + 15] = b[i + 0];
    }
}

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
  char c[n], c2[n];
  unsigned int u[n], u2[n];
  long long l[n], l2[n];
  double d[n], d2[n];
  float f[n], f2[n];

  for (int i = 0; i < n; i++)
    {
      c[i] = i;
      u[i] = i;
      l[i] = i;
      d[i] = i;
      f[i] = i;
      c2[i] = i;
      u2[i] = i;
      l2[i] = i;
      d2[i] = i;
      f2[i] = i;
    }

  reversec (c2, c, n);
  reversei (u2, u, n);
  reversel (l2, l, n);
  reversed (d2, d, n);
  reversef (f2, f, n);

  for (int i = 0; i < n - 16; i++)
    {
      assert (c[i] == c2[i / (16 / sizeof (c[0])) * (16 / sizeof (c[0])) + 16 / sizeof (c[0]) - 1 - i % (16 / sizeof (c[0]))]);
      assert (u[i] == u2[i / (16 / sizeof (u[0])) * (16 / sizeof (u[0])) + 16 / sizeof (u[0]) - 1 - i % (16 / sizeof (u[0]))]);
      assert (l[i] == l2[i / (16 / sizeof (l[0])) * (16 / sizeof (l[0])) + 16 / sizeof (l[0]) - 1 - i % (16 / sizeof (l[0]))]);
      assert (d[i] == d2[i / (16 / sizeof (d[0])) * (16 / sizeof (d[0])) + 16 / sizeof (d[0]) - 1 - i % (16 / sizeof (d[0]))]);
      assert (f[i] == f2[i / (16 / sizeof (f[0])) * (16 / sizeof (f[0])) + 16 / sizeof (f[0]) - 1 - i % (16 / sizeof (f[0]))]);
    }
}
