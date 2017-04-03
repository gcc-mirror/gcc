/* { dg-require-ifunc "" } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2" } */

static double bar (double *__restrict, double *__restrict, int)
__attribute__ ((target_clones("avx,avx2,avx512f,default")));

double
foo (double *__restrict a, double *__restrict b, int n)
{
  return bar (a,b,n);
}

double
bar (double *__restrict a, double *__restrict b, int n)
{
  double s;
  int i;
  s = 0.0;
  for (i=0; i<n; i++)
    s += a[i] + b[i];

  return s;
}

#define N 5

int main ()
{
  double a[N] = {1.2f, 1.2f, 1.2f, 1.2f, 1.2f };
  double b[N] = {1.2f, 1.2f, 1.2f, 1.2f, 1.2f };

  __builtin_printf ("value: %.5f\n", foo (a, b, N));
  return 0;
}
