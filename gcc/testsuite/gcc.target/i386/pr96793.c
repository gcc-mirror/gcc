/* PR target/96793 */
/* { dg-do run { target sse2_runtime } } */
/* { dg-require-effective-target fenv } */
/* { dg-options "-O2 -frounding-math -msse2 -mno-sse4 -mfpmath=sse" } */

#include <fenv.h>

double
__attribute__((noinline))
test (double value)
{
  return __builtin_floor (value);
}

int
main ()
{
  double result;

  fesetround (FE_DOWNWARD);

  result = test (0.25);

  if (__builtin_signbit (result) != 0)
    __builtin_abort ();

  return 0;
}
