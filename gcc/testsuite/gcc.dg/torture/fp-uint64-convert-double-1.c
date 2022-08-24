/* PR84407 */
/* { dg-do run } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target hard_float } */
/* { dg-additional-options "-frounding-math -fexcess-precision=standard" } */

#include <fenv.h>
#include <stdlib.h>

void __attribute__((noipa))
fooa ()
{
#if __DBL_MANT_DIG__ == 53
#ifdef FE_TONEAREST
  fesetround(FE_TONEAREST);
  __UINT64_TYPE__ x = 0x7fffffffffffffff;
  double f = x;
  if (f != 0x1p+63)
    abort ();
#endif
#endif
}

void __attribute__((noipa))
foob ()
{
#if __DBL_MANT_DIG__ == 53
#ifdef FE_DOWNWARD
  fesetround(FE_DOWNWARD);
  __UINT64_TYPE__ x = 0x7fffffffffffffff;
  double f = x;
  if (f != 0x1.fffffffffffffp+62)
    abort ();
#endif
#endif
}

void __attribute__((noipa))
fooc ()
{
#if __DBL_MANT_DIG__ == 53
#ifdef FE_UPWARD
  fesetround(FE_UPWARD);
  __UINT64_TYPE__ x = 0x7fffffffffffffff;
  double f = x;
  if (f != 0x1p+63)
    abort ();
#endif
#endif
}

void __attribute__((noipa))
food ()
{
#if __DBL_MANT_DIG__ == 53
#ifdef FE_TOWARDZERO
  fesetround(FE_TOWARDZERO);
  __UINT64_TYPE__ x = 0x7fffffffffffffff;
  double f = x;
  if (f != 0x1.fffffffffffffp+62)
    abort ();
#endif
#endif
}


int
main ()
{
  fooa ();
  foob ();
  fooc ();
  food ();
  return 0;
}
