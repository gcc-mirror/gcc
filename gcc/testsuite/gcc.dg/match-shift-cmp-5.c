/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 1;" 4 "optimized" { target bitint575 } } } */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "optimized" { target { ! bitint575 } } } } */
/* { dg-final { scan-tree-dump-not " << " "optimized" } } */

bool
foo (unsigned long long x, unsigned y)
{
  if (x >= 64 || x == 0)
    __builtin_unreachable ();
  if (y > sizeof (unsigned long long) * __CHAR_BIT__ - 6)
    __builtin_unreachable ();
  return (x << y) >= y;
}

#if __BITINT_MAXWIDTH__ >= 575
bool
bar (unsigned _BitInt(575) x, unsigned y)
{
  if (x >= 1361129467683753853853498429727072845823uwb || x == 0)
    __builtin_unreachable ();
  if (y > 575 - 130)
    __builtin_unreachable ();
  return (x << y) > y;
}

bool
baz (unsigned _BitInt(575) x, unsigned y)
{
  if (x >= 1361129467683753853853498429727072845823uwb || x == 0)
    __builtin_unreachable ();
  if (y >= 575 - 130)
    __builtin_unreachable ();
  return ((signed _BitInt(575)) (x << y)) > y;
}
#endif

bool
qux (int x, int y)
{
  if (x >= 128 || x <= 0)
    __builtin_unreachable ();
  if (y >= sizeof (int) * __CHAR_BIT__ - 7)
    __builtin_unreachable ();
  return (x << y) >= y;
}
