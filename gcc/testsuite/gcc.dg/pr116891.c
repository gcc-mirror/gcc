/* PR middle-end/116891 */
/* { dg-do run } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target hard_float } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -frounding-math" } */

#include <fenv.h>

__attribute__((noipa)) double
foo (double x, double y, double z)
{
  return -__builtin_fma (-x, y, -z);
}

__attribute__((noipa)) double
bar (double x, double y, double z)
{
  return -__builtin_fma (-x, y, z);
}

__attribute__((noipa)) double
baz (double x, double y, double z)
{
  return -__builtin_fma (x, y, -z);
}

__attribute__((noipa)) double
qux (double x, double y, double z)
{
  return -__builtin_fma (x, y, z);
}

int
main ()
{
#if defined (FE_DOWNWARD) && __DBL_MANT_DIG__ == 53 && __DBL_MAX_EXP__ == 1024
  fesetround (FE_DOWNWARD);
  double a = foo (-0x1.p256, 0x1.p256, 0x1.p-256);
  if (a != -__builtin_nextafter (0x1p256 * 0x1p256, 0.))
    __builtin_abort ();
  if (a != bar (-0x1.p256, 0x1.p256, -0x1.p-256)
      || a != baz (0x1.p256, 0x1.p256, 0x1.p-256)
      || a != qux (0x1.p256, 0x1.p256, -0x1.p-256))
    __builtin_abort ();
#endif
}
