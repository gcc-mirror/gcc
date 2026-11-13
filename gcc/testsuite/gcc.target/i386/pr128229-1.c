/* PR target/128229 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mfpmath=sse -fno-math-errno -ftrapping-math" } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-require-effective-target sse2_runtime } */

#include <fenv.h>

/* lrint expands to cvtsd2si (UNSPEC_FIX_NOTRUNC).  It is loop invariant
   here, but it must not be hoisted above the guard: executing it on a NaN
   raises FE_INVALID.  */

__attribute__((noipa)) long
foo (double x, int *p, int n)
{
  long s = 0;
  for (int i = 0; i < n; i++)
    if (p[i])
      s += __builtin_lrint (x);
  return s;
}

int p[64];

int
main (void)
{
  feclearexcept (FE_INVALID);
  if (foo (__builtin_nan (""), p, 64) != 0)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
  return 0;
}
