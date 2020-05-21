/* PR target/95169 */
/* { dg-do run { target ia32 } } */
/* { dg-options "-O0 -march=i386 -mtune=generic" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

void
f (double y)
{
  if (__builtin_expect (y == 0.0, 0))
    __builtin_abort ();
}

int
main (void)
{
  double y = __builtin_nan ("");

  feclearexcept (FE_INVALID);

  f (y);

  if (fetestexcept (FE_INVALID))
    __builtin_abort ();

  return 0;
}
