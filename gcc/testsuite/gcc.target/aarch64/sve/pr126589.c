/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_hw } */
/* { dg-options "-O3 -march=armv8.3-a+sve -mautovec-preference=sve-only" } */

#define N 64
double a[N], b[N], c[N];

__attribute__((noipa)) void
mul (double *__restrict cc, double *__restrict aa, double *__restrict bb, int n)
{
  for (int i = 0; i < n; i += 2)
    {
      cc[i] = aa[i] * bb[i] - aa[i + 1] * bb[i + 1];
      cc[i + 1] = aa[i] * bb[i + 1] + aa[i + 1] * bb[i];
    }
}

int
main (void)
{
  for (int i = 0; i < N; i += 2)
    {
      a[i] = -0.0;
      a[i + 1] = 0.0;
      b[i] = 1.0;
      b[i + 1] = 1.0;
    }

  mul (c, a, b, N);

  for (int i = 0; i < N; i += 2)
    if (!__builtin_signbit (c[i]))
      __builtin_abort ();

  return 0;
}
