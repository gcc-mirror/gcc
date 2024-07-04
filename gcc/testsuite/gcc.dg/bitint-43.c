/* PR tree-optimization/112719 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 252
int
foo (unsigned _BitInt(239) x, unsigned _BitInt(252) y)
{
  x &= 0x2aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaauwb;
  y &= 0x555555555555555555555555555555555555555555555555555555555555555uwb;
  return __builtin_popcountg (x) + __builtin_popcountg (y);
}

int
bar (unsigned _BitInt(239) x, unsigned _BitInt(252) y)
{
  return __builtin_parityg (x) ^ __builtin_parityg (y);
}
#endif
