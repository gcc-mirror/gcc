/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mlasx -fno-vect-cost-model -fno-unroll-loops" } */
/* { dg-final { scan-assembler-not "xvpickve.w" } } */
/* { dg-final { scan-assembler-not "xvpickve.d" } } */

float
sum_float (float *a, int n) {
  float res = 0.0;
  for (int i = 0; i < n; i++)
    res += a[i];
  return res;
}

double
sum_double (double *a, int n) {
  double res = 0.0;
  for (int i = 0; i < n; i++)
    res += a[i];
  return res;
}
