/* PR target/127177 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mno-avx -ftrapping-math" } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-additional-options "-mfpmath=sse" { target ia32 } } */

#include <fenv.h>

float a[64], b[64];
int r[64];

__attribute__((noipa)) void
t_islessequal (int *r, float *a, float *b, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = __builtin_islessequal (a[i], b[i]);
}

int
main (void)
{
  for (int i = 0; i < 64; i++)
    {
      a[i] = __builtin_nanf ("");	/* quiet NaN */
      b[i] = 1.0f;
    }

  /* A qNaN operand must not raise FE_INVALID here.  */
  feclearexcept (FE_INVALID);
  t_islessequal (r, a, b, 64);
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();

  for (int i = 0; i < 64; i++)
    if (r[i] != 0)
      __builtin_abort ();

  return 0;
}
