/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */
/* { dg-additional-options "-msse -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-additional-options "-mdouble=64" { target { avr-*-* } } } */

double foo (double x)
{
  double one = 1.;
  return __builtin_copysign (x, one);
}
double bar (double x)
{
  double minuszero = -0.;
  return __builtin_copysign (x, minuszero);
}

/* { dg-final { scan-tree-dump-times "__builtin_copysign" 1 "cddce1" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 1 "cddce1" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "= -" 1 "cddce1" { target { ! ifn_copysign } } } } */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 2 "cddce1" { target { ! ifn_copysign } } } } */
