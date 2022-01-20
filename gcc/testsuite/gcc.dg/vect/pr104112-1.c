/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-additional-options "-march=armv8.2-a+sve -msve-vector-bits=512" { target aarch64-*-* } } */

void
boom(int n, double *a, double *x)
{
  int i, j;
  double temp;

  for (j = n; j >= 1; --j)
    {
      temp = x[j];
      for (i = j - 1; i >= 1; --i)
	temp += a[i + j] * x[i];
      x[j] = temp;
    }
}
