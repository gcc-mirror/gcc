/* PR c/96571 */
/* { dg-do compile } */
/* { dg-options "-std=c99 -O2 -Wunused-but-set-variable" } */

enum E { V };

int
foo (void)
{
  enum E v;				/* { dg-bogus "set but not used" } */
  return _Generic (v, enum E : 0);
}

int
bar (void)
{
  int a = 0;				/* { dg-bogus "set but not used" } */
  return _Generic (0, int : a);
}

int
baz (void)
{
  int a;				/* { dg-bogus "set but not used" } */
  return _Generic (0, long long : a, int : 0);
}

int
qux (void)
{
  int a;				/* { dg-bogus "set but not used" } */
  return _Generic (0, long long : a, default: 0);
}
