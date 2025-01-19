/* Test _Decimal64x in C23 mode - float.h macros.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#ifndef __STDC_WANT_IEC_60559_TYPES_EXT__
#define __STDC_WANT_IEC_60559_TYPES_EXT__ 1
#endif

#include <float.h>

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)
static_assert (expr_has_type (DEC64X_MAX, _Decimal64x));
static_assert (expr_has_type (DEC64X_MIN, _Decimal64x));
static_assert (expr_has_type (DEC64X_EPSILON, _Decimal64x));
static_assert (expr_has_type (DEC64X_TRUE_MIN, _Decimal64x));

int
main ()
{
#if __DEC64X_MANT_DIG__ == __DEC128_MANT_DIG__ \
    && __DEC64X_MAX_EXP__ == __DEC128_MAX_EXP__
  if (DEC64X_MANT_DIG != 34)
    __builtin_abort ();

  if (DEC64X_MIN_EXP != -6142)
    __builtin_abort ();

  if (DEC64X_MAX_EXP != 6145)
    __builtin_abort ();

  if (DEC64X_MAX != 9.999999999999999999999999999999999E6144D64x)
    __builtin_abort ();

  if (DEC64X_EPSILON != 1E-33D64x)
    __builtin_abort ();
  
  if (DEC64X_MIN != 1E-6143D64x)
    __builtin_abort ();

  if (DEC64X_TRUE_MIN != 0.000000000000000000000000000000001E-6143D64x)
    __builtin_abort ();
#endif
}
