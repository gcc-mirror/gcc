/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

/* signbit (x) cmp signbit (y) ? y : -y is copysign (y, +-x).  */

double d_eq (double x, double y)
{ return __builtin_signbit (x) == __builtin_signbit (y) ? y : -y; }

double d_eq_swapped (double x, double y)
{ return __builtin_signbit (y) == __builtin_signbit (x) ? y : -y; }

double d_ne (double x, double y)
{ return __builtin_signbit (x) != __builtin_signbit (y) ? -y : y; }

double d_if (double x, double y)
{
  if (__builtin_signbit (x) == __builtin_signbit (y))
    return y;
  return -y;
}

float f_eq (float x, float y)
{ return __builtin_signbitf (x) == __builtin_signbitf (y) ? y : -y; }

/* copysign (y, -x).  */

double d_neg (double x, double y)
{ return __builtin_signbit (x) != __builtin_signbit (y) ? y : -y; }

/* { dg-final { scan-tree-dump-times " = \\.COPYSIGN " 6 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-not "signbit" "optimized" { target ifn_copysign } } } */
