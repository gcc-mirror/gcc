/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23 -pedantic-errors" } */

#if __BITINT_MAXWIDTH__ >= 315
_Bool
foo (_BitInt (315) a, _BitInt (315) b, unsigned *c)
{
  if (a < -8 || a > 7)
    __builtin_unreachable ();
  if (b < 0 || b > 63)
    __builtin_unreachable ();
  return __builtin_add_overflow (a, b, c);
}
#else
int i;
#endif
