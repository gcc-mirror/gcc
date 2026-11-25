/* { dg-do run { target arm_v8_3a_complex_neon_hw } } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-O3 -march=armv8.3-a" } */

#define N 64
double a[N], b[N], c[N];

__attribute__((noipa)) void
mul (double *__restrict cc, double *__restrict aa, double *__restrict bb, int n)
{
  for (int i = 0; i < n; i += 4)
    {
      cc[i] = aa[i] * bb[i] - aa[i + 1] * bb[i + 1];
      cc[i + 1] = aa[i + 2] * bb[i + 1] + aa[i + 1] * bb[i];
      cc[i + 2] = aa[i + 2] * bb[i + 2] - aa[i + 3] * bb[i + 3];
      cc[i + 3] = aa[i + 2] * bb[i + 3] + aa[i + 3] * bb[i + 2];
    }
}

__attribute__((optimize (0))) void
ref (double *__restrict cc, double *__restrict aa, double *__restrict bb, int n)
{
  for (int i = 0; i < n; i += 4)
    {
      cc[i] = aa[i] * bb[i] - aa[i + 1] * bb[i + 1];
      cc[i + 1] = aa[i + 2] * bb[i + 1] + aa[i + 1] * bb[i];
      cc[i + 2] = aa[i + 2] * bb[i + 2] - aa[i + 3] * bb[i + 3];
      cc[i + 3] = aa[i + 2] * bb[i + 3] + aa[i + 3] * bb[i + 2];
    }
}

int
main (void)
{
  double e[N];

  for (int i = 0; i < N; ++i)
    {
      a[i] = i + 1;
      b[i] = i * 3 + 1;
    }

  mul (c, a, b, N);
  ref (e, a, b, N);

  for (int i = 0; i < N; ++i)
    if (c[i] != e[i])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-assembler-not {fcmla\t} } } */
