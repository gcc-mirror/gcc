/* PR target/100119 */
/* { dg-do run { target sse2_runtime } } */
/* { dg-require-effective-target fenv } */
/* { dg-options "-O2 -frounding-math -msse2 -mno-avx512f -mfpmath=sse" } */

#include <fenv.h>

double
__attribute__((noinline))
test (unsigned int x)
{
  return x;
}

int
main ()
{
  double result;

  fesetround (FE_DOWNWARD);

  result = test (0);

  if (__builtin_signbit (result) != 0)
    __builtin_abort ();

  return 0;
}
