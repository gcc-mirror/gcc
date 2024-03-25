/* PR tree-optimization/112902 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

double c;
#if __BITINT_MAXWIDTH__ >= 2048
_BitInt (512) a;
_BitInt (2048) b;

void
foo (void)
{
  b = __builtin_mul_overflow_p (40, (_BitInt (512)) (-b * a), 0);
}


void
bar (void)
{
  c -= (unsigned _BitInt (512)) (a | a << b);
}
#endif
