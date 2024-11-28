/* PR c/117802 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=c23" } */

int
foo (float x, _BitInt(8) y)
{
  return __builtin_iseqsig (x, y) * 2 + __builtin_iseqsig (y, x);
}

int
bar (double x, unsigned _BitInt(162) y)
{
  return __builtin_iseqsig (x, y) * 2 + __builtin_iseqsig (y, x);
}

int
baz (long double x, _BitInt(574) y)
{
  return __builtin_iseqsig (x, y) * 2 + __builtin_iseqsig (y, x);
}
