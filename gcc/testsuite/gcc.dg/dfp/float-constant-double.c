/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* Constant float values of type double in <float.h> are suffixed with L
   and cast to double so they can be used within code that uses pragma
   FLOAT_CONST_DECIMAL64.  If they were not suffixed then use of the macro
   would have them interpreted as _Decimal64, leading to errors when used
   in expressions with other operands of type double.  */

#include <float.h>

extern double a, b, c, d;

void
foo ()
{
  _Pragma ("STDC FLOAT_CONST_DECIMAL64 ON")
  a = 0.1d * DBL_MAX;
  b = DBL_EPSILON * 10.0d;
  c = DBL_MIN * 200.0d;
}
