/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target aarch64_sve_hw } */
/* { dg-additional-options "-march=armv8-a+sve" } */

#define N 137
static double a[N], d[N], e[N];
static float fa[N], fd[N], fe[N];

__attribute__((noipa)) void
rev (double *__restrict d, double *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_roundeven (a[i]);
}

__attribute__((noipa, optimize ("O0"))) void
rev_ref (double *__restrict d, double *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_roundeven (a[i]);
}

__attribute__((noipa)) void
revf (float *__restrict d, float *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_roundevenf (a[i]);
}

__attribute__((noipa, optimize ("O0"))) void
revf_ref (float *__restrict d, float *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_roundevenf (a[i]);
}

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = (i - 68) * 0.5 + (i & 3) * 0.25;
      fa[i] = (float) a[i];
    }

  rev (d, a, N);
  rev_ref (e, a, N);
  for (int i = 0; i < N; i++)
    if (d[i] != e[i])
      __builtin_abort ();

  revf (fd, fa, N);
  revf_ref (fe, fa, N);
  for (int i = 0; i < N; i++)
    if (fd[i] != fe[i])
      __builtin_abort ();

  return 0;
}
