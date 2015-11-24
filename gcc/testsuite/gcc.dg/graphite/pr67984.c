/* { dg-options "-O2 -floop-nest-optimize" } */

void foo(int N, float *x, float *X)
{
  float sum = 0.;
  for (int n = 0; n < N; ++n)
    sum += x[n];

  X[0] = sum;

  for (unsigned int k = 1; k < N; ++k) {
    sum = 0.;
    for (int n = 0; n < N; ++n)
      sum += x[n] * ((float)(3.14159265358979323846 * (n + .5) * k / N));
    X[k] = sum;
  }
}
