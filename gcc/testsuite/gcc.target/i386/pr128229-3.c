/* PR target/128229 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mfpmath=sse -fno-math-errno -ftrapping-math" } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-require-effective-target sse2_runtime } */

#include <fenv.h>

/* cvtsd2si (UNSPEC_FIX_NOTRUNC) raises FE_INVALID on a NaN.  The
   conversion is guarded; if-conversion must not speculate it.  */

__attribute__((noipa)) int
foo (double x, int c)
{
  int r = 0;
  if (c)
    r = __builtin_irint (x);
  return r;
}

int
main (void)
{
  feclearexcept (FE_INVALID);
  if (foo (__builtin_nan (""), 0) != 0)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
  return 0;
}
