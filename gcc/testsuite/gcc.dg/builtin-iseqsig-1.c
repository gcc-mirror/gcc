/* { dg-do compile } */
/* { dg-options "" } */

int
foo (float x, int y)
{
  return __builtin_iseqsig (x, y) * 2 + __builtin_iseqsig (y, x);
}

int
bar (double x, unsigned long y)
{
  return __builtin_iseqsig (x, y) * 2 + __builtin_iseqsig (y, x);
}

int
baz (long double x, long long y)
{
  return __builtin_iseqsig (x, y) * 2 + __builtin_iseqsig (y, x);
}
