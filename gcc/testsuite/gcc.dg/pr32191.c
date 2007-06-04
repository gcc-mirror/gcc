/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-std=c99" } */

typedef _Complex float __attribute__((mode(TC))) _Complex128;

_Complex128 foo (_Complex128 x, _Complex128 y)
{
  return x * y;
}
